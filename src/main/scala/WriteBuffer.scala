// Designed by Kyle. 2025-03-29 14:52
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import CONSTANTS._
import CONSTANTS.CONFIG._
import CONSTANTS.FU_TYPE.MEM
/*
    数据写缓存 (Write Buffer)
    访存指令执行时，将数据写入写缓存，直至提交阶段再写入内存
    现在支持有序提交和stall信号处理
    优化：利用顺序访存和顺序提交的特点简化逻辑
 */

class BufferEntry extends Bundle {
    val valid     = Bool()     // 有效位
    val committed = Bool()     // 已提交位 (ROB已提交但未写入RAM)
    val addr      = UInt(32.W) // 读写地址
    val Wdata     = UInt(32.W) // 写入数据
    val WriteSize = UInt(2.W)  // 写入大小
}

class WriteBuffer(nROBEntries: Int, nWBEntries: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val clear      = Input(Bool())
            val ROBIndex   = Input(UInt(log2Up(nROBEntries).W)) // 来自重排序缓存的索引
            val readEnable = Input(Bool())
            val writeEnable = Input(Bool())     // 写使能 (高表示写Buffer，低表示读请求)
            val addr        = Input(UInt(32.W)) // 读写地址
            val Wdata       = Input(UInt(32.W)) // 写入数据 (仅在 writeEnable 为高时有效)
            val writeSize   = Input(UInt(2.W))  // 写入大小 (仅在 writeEnable 为高时有效)

            val readData      = Input(UInt(32.W)) // 来自RAM (对应上一周期发送的地址)
            val responseValid = Input(Bool())     // RAM响应有效 (对应上一周期发送的地址)

            val uart_data_read = Input(Bool())                      // UART数据读取请求
            val retireEnable   = Input(Bool())
            val retireIndex    = Input(UInt(log2Up(nROBEntries).W)) // 提交索引
        }
        val out = new Bundle {
            val reqValid       = Output(Bool())     // 请求有效
            val Rdata          = Output(UInt(32.W)) // 读出数据 (在地址提供后一周期有效)
            val RAMwriteEnable = Output(Bool())     // RAM写使能
            val RAMAddr        = Output(UInt(32.W)) // RAM地址 (读或写)
            val RAMwriteData   = Output(UInt(32.W)) // RAM写数据
            val RAMBen         = Output(UInt(4.W))  // RAM字节使能 (写时使用)
            val bufferFull     = Output(Bool())
            val cacheMiss      = Output(Bool())     // 作为对外的暂停信号
            val uart_data_read = Output(Bool())
        }
    })

    debugSignal(io.in.readData)

    // 优化：简化为FIFO队列结构，利用顺序特性
    val buffer = RegInit(VecInit(Seq.fill(nWBEntries)(0.U.asTypeOf(new BufferEntry))))

    // 队列指针
    val head_ptr = RegInit(0.U(log2Up(nWBEntries).W))     // 指向最老的条目(下一个要写入RAM的)
    val tail_ptr = RegInit(0.U(log2Up(nWBEntries).W))     // 指向下一个空闲位置
    val count    = RegInit(0.U(log2Up(nWBEntries + 1).W)) // 当前条目数

    // 优化：添加commit指针，利用顺序提交特性
    val commit_ptr = RegInit(0.U(log2Up(nWBEntries).W)) // 指向下一个要提交的条目

    val addr_reg      = RegInit(0.U(32.W))
    val writeSize_reg = RegInit(0.U(2.W))

    // --- 新增寄存器 ---
    // 寄存上一周期的读请求是否命中 Buffer
    val w_hit         = WireInit(false.B)
    val r_hit         = RegInit(false.B)
    val r_hit_delayed = RegNext(r_hit, false.B) // 延迟一周期的命中状态
    // 寄存上一周期命中的数据
    val r_hit_data = RegInit(0.U(32.W))
    // -----------------

    // 状态：区分读请求和写请求
    val has_read_req = io.in.readEnable
    val has_write_ready =
        count > 0.U && buffer(head_ptr).valid && buffer(head_ptr).committed

    // 默认输出
    io.out.RAMwriteEnable := false.B
    io.out.RAMAddr        := 0.U
    io.out.RAMwriteData   := 0.U
    io.out.Rdata          := 0.U
    io.out.RAMBen         := "b0000".U
    io.out.reqValid       := false.B
    io.out.cacheMiss      := count === nWBEntries.U
    io.out.uart_data_read := io.in.uart_data_read

    // 命中检测逻辑 (检查所有有效条目)
    val hit_vec = buffer.map(entry =>
        entry.valid &&
            entry.addr(log2Ceil(DataRAMSize), 0) === io.in.addr(log2Ceil(DataRAMSize), 0)
    )
    val is_hit = hit_vec.reduce(_ || _)
    // val hit_data = Mux1H(PriorityEncoderOH(hit_vec), buffer.map(_.Wdata))

    val hit_data = Mux1H(hit_vec, buffer.map(_.Wdata))

    debugSignal(hit_data)
    debugSignal(is_hit)

    def BenGen(writeSize: UInt, addr_lsb: UInt): UInt = {
        // 根据写入大小和地址最低位生成字节使能
        MuxLookup(writeSize, "b1111".U)(
          Seq(
            MEM_SIZE.BYTE     -> (1.U << addr_lsb),
            MEM_SIZE.HALFWORD -> Mux(addr_lsb(1), "b1100".U, "b0011".U),
            MEM_SIZE.WORD     -> "b1111".U
          )
        )
    }

    val hasNew = WireInit(false.B)

    // 写入Buffer操作 (在队列尾部添加新条目)
    when(io.in.writeEnable && count < nWBEntries.U) {
        buffer(tail_ptr).valid     := true.B
        buffer(tail_ptr).committed := false.B
        buffer(tail_ptr).addr      := io.in.addr
        buffer(tail_ptr).Wdata     := io.in.Wdata
        buffer(tail_ptr).WriteSize := io.in.writeSize

        tail_ptr := (tail_ptr + 1.U) % nWBEntries.U
        hasNew   := true.B
    }

    w_hit      := is_hit
    r_hit      := is_hit
    r_hit_data := hit_data

    // 优化：利用顺序提交特性，简化ROB提交操作
    // 修复：优先处理commit信号，避免被clear信号误清除
    val should_commit = io.in.retireEnable && count > 0.U &&
        buffer(commit_ptr).valid && !buffer(commit_ptr).committed

    when(should_commit) {
        buffer(commit_ptr).committed := true.B
    }

    // val sReadIdle :: sReadWait :: sReadProgress :: Nil = Enum(3)
    val sReadIdle :: sReadWait :: sReadProgress :: Nil = Enum(3)
    val readState                                      = RegInit(sReadIdle)
    val sWriteIdle :: sWriteProgress :: Nil            = Enum(2)
    val sWriteState                                    = RegInit(sWriteIdle)

    val Rdata = RegInit(0.U(32.W))

    switch(readState) {
        is(sReadIdle) {
            // when(has_read_req && !is_hit && !io.in.clear) {
            when(has_read_req && !is_hit) {
                addr_reg      := io.in.addr
                writeSize_reg := io.in.writeSize
                when( // 等待写入完成后抢占
                  sWriteState === sWriteIdle || sWriteState === sWriteProgress && io.in.responseValid
                ) {
                    io.out.reqValid := true.B
                    io.out.RAMAddr  := io.in.addr
                    io.out.RAMBen   := BenGen(io.in.writeSize, io.in.addr(1, 0))
                    readState       := sReadProgress
                }.otherwise {
                    readState := sReadWait
                }
            }.elsewhen(is_hit) {
                io.out.reqValid := false.B
            }
            when(r_hit) {
                io.out.Rdata := r_hit_data
            }
        }
        is(sReadWait) {
            io.out.cacheMiss := true.B
            when(io.in.responseValid) {
                io.out.reqValid := true.B
                io.out.RAMAddr  := addr_reg
                io.out.RAMBen   := BenGen(writeSize_reg, addr_reg(1, 0))
                readState       := sReadProgress
            }
        }
        is(sReadProgress) {
            when(io.in.responseValid) {
                // 根据地址和写入大小截取对应的8位字节，扩展到32位
                io.out.Rdata := MuxCase(
                  io.in.readData,
                  Seq(
                    (writeSize_reg === MEM_SIZE.BYTE) -> Cat(
                      Fill(24, 0.U),
                      (io.in.readData >> (addr_reg(1, 0) << 3))(7, 0)
                    ),
                    (writeSize_reg === MEM_SIZE.HALFWORD) -> Cat(
                      Fill(16, 0.U),
                      (io.in.readData >> (addr_reg(1) << 3))(15, 0)
                    )
                  )
                )
                // when(has_read_req && !is_hit && !io.in.clear) { // 连续地进行下一次读取
                when(has_read_req && !is_hit) {
                    addr_reg        := io.in.addr
                    writeSize_reg   := io.in.writeSize
                    io.out.reqValid := true.B
                    io.out.RAMAddr  := io.in.addr
                    io.out.RAMBen   := BenGen(io.in.writeSize, io.in.addr(1, 0))
                    readState       := sReadProgress
                }.otherwise {
                    readState := sReadIdle
                }
            }.otherwise {
                io.out.cacheMiss := true.B
                io.out.reqValid  := true.B
                io.out.RAMAddr   := addr_reg
                io.out.RAMBen    := BenGen(writeSize_reg, addr_reg(1, 0))
            }
            // when(io.in.clear) {
            //     readState        := sReadIdle
            //     io.out.Rdata     := 0.U
            //     io.out.cacheMiss := false.B
            // }
        }
    }

    // io.out.Rdata := Mux(r_hit, r_hit_data, Rdata)

    // RAM写入逻辑：只有在满足条件时才写入
    // 条件：1) 有已提交的条目 2) !stall 3) 没有读请求
    val reg_head_entry = RegInit(0.U.asTypeOf(buffer.head))
    val hasLeft        = WireInit(false.B)

    def OutputWriteInfo(head_entry: BufferEntry): Unit = {
        io.out.RAMAddr      := head_entry.addr
        io.out.RAMwriteData := head_entry.Wdata

        // 根据写入大小计算写掩码
        val addr_lsb = head_entry.addr(1, 0)
        io.out.RAMBen         := BenGen(head_entry.WriteSize, addr_lsb)
        io.out.reqValid       := true.B
        io.out.RAMwriteEnable := true.B
    }

    debugSignal(has_write_ready)
    debugSignal(has_read_req)

    switch(sWriteState) {
        is(sWriteIdle) {
            when(has_write_ready && !has_read_req && readState === sReadIdle) {
                val head_entry = buffer(head_ptr)
                reg_head_entry             := head_entry
                hasLeft                    := true.B
                buffer(head_ptr).valid     := false.B
                buffer(head_ptr).committed := false.B
                OutputWriteInfo(head_entry)
                sWriteState := sWriteProgress
            }
        }
        is(sWriteProgress) {
            when(io.in.responseValid) {
                when(has_write_ready && !has_read_req && readState === sReadIdle) {
                    val head_entry = buffer(head_ptr)
                    hasLeft                    := true.B
                    reg_head_entry             := head_entry
                    buffer(head_ptr).valid     := false.B
                    buffer(head_ptr).committed := false.B
                    OutputWriteInfo(head_entry) // 连续地进行下一次写入
                }.otherwise {
                    sWriteState := sWriteIdle
                }
            }.otherwise {
                OutputWriteInfo(reg_head_entry)
            }
        }
    }

    when(io.in.clear) {
        val newValid = VecInit(Seq.fill(nWBEntries)(false.B))
        for (i <- 0 until nWBEntries) {
            val will_be_committed = should_commit && (i.U === commit_ptr)
            newValid(i) := buffer(i).valid &&
                (buffer(i).committed || will_be_committed) &&
                !(i.U === head_ptr && hasLeft)
            buffer(i).valid := newValid(i)
        }

        // val remaining_committed = PopCount(buffer.zipWithIndex.map { case (entry, i) =>
        //     val will_be_committed = should_commit && (i.U === commit_ptr)
        //     entry.valid && (entry.committed || will_be_committed) &&
        //     !(i.U === head_ptr && hasLeft)
        // })

        // 更新指针：所有未提交的条目被清除，只保留已提交的
        head_ptr   := (head_ptr + hasLeft.asUInt)         % nWBEntries.U
        commit_ptr := (commit_ptr + should_commit.asUInt) % nWBEntries.U
        tail_ptr   := (commit_ptr + should_commit.asUInt) % nWBEntries.U
        count      := PopCount(newValid)

        r_hit := false.B
        w_hit := false.B
    }.otherwise {
        head_ptr   := (head_ptr + hasLeft.asUInt)         % nWBEntries.U
        commit_ptr := (commit_ptr + should_commit.asUInt) % nWBEntries.U
        tail_ptr   := (tail_ptr + hasNew.asUInt)          % nWBEntries.U
        count      := count - hasLeft.asUInt + hasNew.asUInt
    }

    debugSignal(hasLeft)
    debugSignal(hasNew)

    io.out.bufferFull := count === nWBEntries.U

    if (advancedDebug) {
        val cacheMissCounter = RegInit(0.U(8.W))
        when(io.out.cacheMiss) {
            cacheMissCounter := Mux(
              cacheMissCounter === 255.U,
              255.U,
              cacheMissCounter + 1.U
            )
        }.otherwise {
            cacheMissCounter := 0.U
        }

        debugSignal(cacheMissCounter)
    }

    if (PERF_STATS) {
        val memRequests = RegInit(0.U(32.W))
        val timeTaken = RegInit(0.U(32.W))

        when(io.in.readEnable || io.in.writeEnable) {
            memRequests := memRequests + 1.U
            timeTaken := timeTaken + 1.U
        }

        when(io.out.cacheMiss) {
            timeTaken := timeTaken + 1.U
        }

        debugSignal(memRequests)
        debugSignal(timeTaken)
    }
}

object WriteBuffer extends App {
    ChiselStage.emitSystemVerilogFile(
      new WriteBuffer(nROBEntries = 64, nWBEntries = 8),
      Array("--target-dir", "generated")
    )
}
