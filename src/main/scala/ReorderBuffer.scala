// Designed by Kyle. 2025-03-15 16:37
import chisel3._
import circt.stage.ChiselStage
import CONSTANTS.CONFIG.PERF_STATS
import chisel3.util._
import CONSTANTS.CONFIG.branchUpdateBypass
import CONSTANTS.CONFIG.debugSignal

/*
    重排序缓存 (ROB)
    使用 FIFO 结构维护正在执行过程中的指令的状态，最多一个周期写入四条指令，完成四条指令的更新，退休四条指令
 */

class BranchUpdateQueue(depth: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val valid             = Input(Vec(4, Bool()))
            val PC_where_branched = Input(Vec(4, UInt(32.W)))
            val direction_true    = Input(Vec(4, Bool()))
            val target_true       = Input(Vec(4, UInt(32.W)))
            val branch_type       = Input(Vec(4, UInt(2.W)))
            val clear             = Input(Bool())
        }
        val out = new Bundle {
            val queueFull         = Output(Bool())
            val valid             = Output(Bool())
            val PC_where_branched = Output(UInt(32.W))
            val direction_true    = Output(Bool())
            val target_true       = Output(UInt(32.W))
            val branch_type       = Output(UInt(2.W))
        }
    })
    // 定义队列表项
    class Entry extends Bundle {
        val PC_where_branched = UInt(32.W)
        val direction_true    = Bool()
        val target_true       = UInt(32.W)
        val branch_type       = UInt(2.W)
    }

    // 队列存储
    val queue = RegInit(VecInit(Seq.fill(depth)(0.U.asTypeOf(new Entry))))
    val valid = RegInit(VecInit(Seq.fill(depth)(false.B)))
    val head  = RegInit(0.U(log2Ceil(depth).W))
    val tail  = RegInit(0.U(log2Ceil(depth).W))
    val used  = RegInit(0.U((log2Ceil(depth) + 1).W))

    // 多输入写入
    val in_valids = io.in.valid
    val num_push  = PopCount(in_valids)
    val can_push  = (used + num_push) <= depth.U

    // 检查是否空
    val empty = used === 0.U

    // 直接旁路条件：队列为空且有一个输入
    val bypass = Wire(Bool())
    if (branchUpdateBypass) {
        bypass := empty && (PopCount(in_valids) === 1.U)
    } else {
        bypass := false.B
    }

    // 输出端口默认
    io.out.valid             := false.B
    io.out.PC_where_branched := 0.U
    io.out.direction_true    := false.B
    io.out.target_true       := 0.U
    io.out.branch_type       := 0.U
    io.out.queueFull         := !can_push

    when(io.in.clear) {
        for (i <- 0 until depth) {
            queue(i) := 0.U.asTypeOf(new Entry)
            valid(i) := false.B
        }
        head := 0.U
        tail := 0.U
        used := 0.U
    }

    // 写入逻辑
    when(!io.in.clear && can_push && in_valids.asUInt.orR && !bypass) {
        val offsets = Wire(Vec(4, UInt(log2Ceil(4).W)))
        offsets(0) := 0.U
        offsets(1) := in_valids(0).asUInt
        offsets(2) := in_valids(0).asUInt + in_valids(1).asUInt
        offsets(3) := in_valids(0).asUInt + in_valids(1).asUInt + in_valids(2).asUInt

        for (i <- 0 until 4) {
            when(in_valids(i)) {
                val write_idx = (tail + offsets(i)) % depth.U
                queue(write_idx).PC_where_branched := io.in.PC_where_branched(i)
                queue(write_idx).direction_true    := io.in.direction_true(i)
                queue(write_idx).target_true       := io.in.target_true(i)
                queue(write_idx).branch_type       := io.in.branch_type(i)
                valid(write_idx)                   := true.B
            }
        }
        tail := (tail + num_push) % depth.U
        used := used + num_push
    }

    // 旁路输出
    when(bypass) {
        val firstIdx = PriorityEncoder(in_valids)
        io.out.valid             := true.B
        io.out.PC_where_branched := io.in.PC_where_branched(firstIdx)
        io.out.direction_true    := io.in.direction_true(firstIdx)
        io.out.target_true       := io.in.target_true(firstIdx)
        io.out.branch_type       := io.in.branch_type(firstIdx)
        // 不写入队列，不移动指针
    }.elsewhen(!empty && valid(head)) {
        // 正常出队
        io.out.valid             := true.B
        io.out.PC_where_branched := queue(head).PC_where_branched
        io.out.direction_true    := queue(head).direction_true
        io.out.target_true       := queue(head).target_true
        io.out.branch_type       := queue(head).branch_type
        valid(head)              := false.B
        head                     := Mux(head === (depth - 1).U, 0.U, head + 1.U)
        used                     := used - 1.U
    }
}

class ReorderBuffer(nROBEntries: Int, nPsyRegs: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val records = Input(
              Vec(
                4,
                new Bundle {
                    val srcL        = UInt(log2Ceil(nPsyRegs).W)
                    val valL        = Bool()
                    val srcR        = UInt(log2Ceil(nPsyRegs).W)
                    val valR        = Bool()
                    val dest        = UInt(log2Ceil(nPsyRegs).W)
                    val dest_origin = UInt(5.W)
                    val dest_old    = UInt(log2Ceil(nPsyRegs).W)
                    val mem_write   = Input(Bool())
                }
              )
            )
            val misPredict      = Input(Bool())
            val record_valid    = Input(Vec(4, Bool()))
            val commit_indexes  = Input(Vec(4, UInt(log2Up(nROBEntries).W)))
            val commit_enable   = Input(Vec(4, Bool()))
            val lastValidID     = Input(UInt(log2Up(nROBEntries).W)) // 最后一个有效指令的索引
            val writeBufferFull = Input(Bool())                      // 写缓冲区满
            val pauseWriteROB   = Input(Bool())                      // 暂停写入ROB
            val clear           = Input(Bool())                      // 重置信号
            val uart_data_read  = Input(Bool())
            val branch_info = Input(new Bundle {
                val valid             = Vec(2, Bool())
                val PC_where_branched = Vec(2, UInt(32.W))
                val direction_true    = Vec(2, Bool())
                val target_true       = Vec(2, UInt(32.W))
                val branch_type       = Vec(2, UInt(2.W))
            })
        }
        val out = new Bundle {
            val record_indexes = Output(Vec(4, UInt(log2Up(nROBEntries).W))) // 记录索引
            val insert_valid   = Output(Vec(4, Bool()))                      // 插入有效信号
            val commit_register =
                Output(Vec(4, UInt(log2Ceil(nPsyRegs).W))) // 提交至寄存器重命名模块
            val dest_origin            = Output(Vec(4, UInt(5.W)))
            val dest_old               = Output(Vec(4, UInt(log2Ceil(nPsyRegs).W)))
            val commit_register_enable = Output(Vec(4, Bool()))
            val head_pointer           = Output(UInt(log2Up(nROBEntries).W))
            val tail_pointer           = Output(UInt(log2Up(nROBEntries).W))
            val ROB_full               = Output(Bool())
            val ROB_empty              = Output(Bool())
            val writeBufferIndex       = Output(UInt(log2Up(nROBEntries).W))
            val writeBufferValid       = Output(Bool())
            val uart_commit            = Output(Bool())
            val branch_info = new Bundle {
                val valid             = Output(Bool())
                val PC_where_branched = Output(UInt(32.W))
                val direction_true    = Output(Bool())
                val target_true       = Output(UInt(32.W))
                val branch_type       = Output(UInt(2.W))
            }
        }
    })

    // val misPredict  = RegNext(io.in.misPredict)
    // val branch_info = RegNext(io.in.branch_info)
    // val last_valid_id = RegNext(io.in.lastValidID)
    val misPredict    = io.in.misPredict
    val branch_info   = io.in.branch_info
    val last_valid_id = io.in.lastValidID

    val ROB = RegInit(VecInit(Seq.fill(nROBEntries)(0.U.asTypeOf(new Bundle {
        val valid             = Bool()
        val commited          = Bool()
        val srcL              = UInt(log2Ceil(nPsyRegs).W)
        val valL              = Bool()
        val srcR              = UInt(log2Ceil(nPsyRegs).W)
        val valR              = Bool()
        val dest              = UInt(log2Ceil(nPsyRegs).W)
        val dest_origin       = UInt(5.W)
        val dest_old          = UInt(log2Ceil(nPsyRegs).W)
        val mem_write         = Bool()
        val branch_valid      = Bool()
        val PC_where_branched = UInt(32.W)
        val direction_true    = Bool()
        val target_true       = UInt(32.W)
        val branch_type       = UInt(2.W)
        val uart_data_read    = Bool()
    }))))

    val branchUpdateQueue   = Module(new BranchUpdateQueue(8))
    val reg_record_indexes  = RegInit(VecInit(Seq.fill(4)(0.U(log2Up(nROBEntries).W))))
    val reg_commit_register = RegInit(VecInit(Seq.fill(4)(0.U(log2Ceil(nPsyRegs).W))))
    val reg_commit_register_origin = RegInit(VecInit(Seq.fill(4)(0.U(5.W))))
    val reg_commit_register_old = RegInit(VecInit(Seq.fill(4)(0.U(log2Ceil(nPsyRegs).W))))
    val reg_commit_register_enable = RegInit(VecInit(Seq.fill(4)(false.B)))
    val reg_uart_commit            = RegInit(false.B) // 用于UART数据提交
    val reg_to_be_freed  = RegInit(VecInit(Seq.fill(4)(0.U(log2Ceil(nPsyRegs).W))))
    val head_pointer     = RegInit(0.U(log2Up(nROBEntries).W))
    val tail_pointer     = RegInit(0.U(log2Up(nROBEntries).W))
    val lastValidId      = RegInit(0.U(log2Up(nROBEntries).W))
    val writeBufferIndex = RegInit(0.U(log2Up(nROBEntries).W))
    val writeBufferValid = RegInit(false.B)
    val recoverMode      = RegInit(false.B) // 预测错误模式

    val mask              = RegInit(false.B)
    val clearDelayCounter = RegInit(0.U(3.W)) // 用于清除延迟

    when(mask === false.B) {
        when(io.in.clear) {
            clearDelayCounter := 0.U
            mask              := true.B
        }
    }.otherwise {
        clearDelayCounter := clearDelayCounter + 1.U
        when(clearDelayCounter === 5.U) {
            mask := false.B
        }
    }

    // 计算队列使用量
    val used_entries = Mux(
      tail_pointer >= head_pointer,
      tail_pointer - head_pointer,
      nROBEntries.U - head_pointer + tail_pointer
    )

    // val ROB_full = used_entries +& PopCount(io.in.record_valid) >= (nROBEntries - 4).U
    val ROB_full = used_entries +& PopCount(io.in.record_valid) >= nROBEntries.U

    for (i <- 0 until 4) {
        io.out.insert_valid(i) := !io.in.pauseWriteROB && io.in.record_valid(i)
    }

    // Retire instructions based on how many can be retired
    def isAfterLastValid(idx: UInt, lastValidID: UInt): Bool = {
        Mux(
          lastValidID <= tail_pointer,
          (idx > lastValidID) && (idx < tail_pointer),
          (idx > lastValidID) || (idx < tail_pointer)
        )
    }

    // when(!recoverMode && misPredict) {
    when(misPredict) {
        recoverMode := true.B
        lastValidId := last_valid_id
        for (i <- 0 until nROBEntries) {
            when(ROB(i).valid && isAfterLastValid(i.U, last_valid_id)) {
                ROB(i).valid    := false.B // 清除所有在lastValidId之后的指令
                ROB(i).commited := false.B
            }
        }
        tail_pointer := last_valid_id + 1.U
    }

    debugSignal(lastValidId)

    // 在for循环外部计算需要添加多少个条目
    when(!reset.asBool && !io.in.pauseWriteROB && !io.in.clear && !ROB_full) {
        val valid_count = io.in.record_valid.map(x => x.asUInt).reduce(_ +& _)
        // 更新尾指针，一次性增加所有有效指令数量
        tail_pointer := (tail_pointer + valid_count) % nROBEntries.U
    }

    // 在循环内只写入数据，不更新指针
    when(!reset.asBool && !io.in.pauseWriteROB && !io.in.clear && !ROB_full) {
        for (i <- 0 until 4) {
            when(io.in.record_valid(i)) {
                // 统计该指令之前有多少条有效指令
                val prev_valid_count = io.in.record_valid
                    .take(i)
                    .map(_.asUInt)
                    .reduceOption(_ +& _)
                    .getOrElse(0.U)
                val write_idx = (tail_pointer + prev_valid_count) % nROBEntries.U
                debugSignal(write_idx)
                ROB(write_idx).valid       := true.B
                ROB(write_idx).srcL        := io.in.records(i).srcL
                ROB(write_idx).valL        := io.in.records(i).valL
                ROB(write_idx).srcR        := io.in.records(i).srcR
                ROB(write_idx).valR        := io.in.records(i).valR
                ROB(write_idx).dest        := io.in.records(i).dest
                ROB(write_idx).dest_old    := io.in.records(i).dest_old
                ROB(write_idx).dest_origin := io.in.records(i).dest_origin
                ROB(write_idx).mem_write   := io.in.records(i).mem_write
                reg_record_indexes(i)      := write_idx
            }
        }
    }

    when(!reset.asBool && !io.in.clear && !mask) {
        for (i <- 0 until 4) {
            when(io.in.commit_enable(i)) { // 完成执行过程，提交至ROB
                ROB(io.in.commit_indexes(i)).commited := true.B
                if (i < 2) {
                    ROB(io.in.commit_indexes(i)).branch_valid := branch_info.valid(i)
                    ROB(io.in.commit_indexes(i)).PC_where_branched := branch_info
                        .PC_where_branched(i)
                    ROB(io.in.commit_indexes(i)).direction_true := branch_info
                        .direction_true(i)
                    ROB(io.in.commit_indexes(i)).target_true := branch_info.target_true(i)
                    ROB(io.in.commit_indexes(i)).branch_type := branch_info.branch_type(i)
                }
                if (i != 2) {
                    ROB(io.in.commit_indexes(i)).uart_data_read := false.B
                }
            }
        }
        ROB(io.in.commit_indexes(2)).uart_data_read := RegNext(
          io.in.uart_data_read
        ) // 访存类指令
    }

    // Reset retirement registers at the beginning of each cycle
    for (i <- 0 until 4) {
        reg_commit_register(i)        := 0.U
        reg_commit_register_origin(i) := 0.U
        reg_commit_register_enable(i) := false.B
        reg_commit_register_old(i)    := 0.U
    }

    // Calculate indices for the next 4 instructions
    val idx0 = head_pointer
    val idx1 = (head_pointer + 1.U) % nROBEntries.U
    val idx2 = (head_pointer + 2.U) % nROBEntries.U
    val idx3 = (head_pointer + 3.U) % nROBEntries.U

    // 检查每个可能退休的指令是否有内存写操作
    val mem_write0 = ROB(idx0).mem_write
    val mem_write1 = ROB(idx1).mem_write
    val mem_write2 = ROB(idx2).mem_write
    val mem_write3 = ROB(idx3).mem_write

    // 检查基本退休条件
    val retire_ready0 = ROB(idx0).valid && ROB(idx0).commited &&
        !(io.in.misPredict && isAfterLastValid(idx0, io.in.lastValidID))

    val retire_ready1 = ROB(idx1).valid && ROB(idx1).commited &&
        retire_ready0 && !(io.in.misPredict && isAfterLastValid(idx1, io.in.lastValidID))

    val retire_ready2 = ROB(idx2).valid && ROB(idx2).commited &&
        retire_ready1 && !(io.in.misPredict && isAfterLastValid(idx2, io.in.lastValidID))
    
    val retire_ready3 = ROB(idx3).valid && ROB(idx3).commited &&
        retire_ready2 && !(io.in.misPredict && isAfterLastValid(idx3, io.in.lastValidID))

    // 根据内存写指令约束确定最终可退休的指令
    val can_retire0 = retire_ready0
    val can_retire1 = retire_ready1 && !mem_write0                // 如果指令0是内存写，指令1不能退休
    val can_retire2 = retire_ready2 && !mem_write0 && !mem_write1 // 如果指令0或1是内存写，指令2不能退休
    val can_retire3 =
        retire_ready3 && !mem_write0 && !mem_write1 && !mem_write2 // 如果之前有内存写，指令3不能退休

    debugSignal(retire_ready0)
    debugSignal(retire_ready1)
    debugSignal(retire_ready2)
    debugSignal(retire_ready3)
    debugSignal(can_retire0)
    debugSignal(can_retire1)
    debugSignal(can_retire2)
    debugSignal(can_retire3)

    // 设置writeBufferIndex和writeBufferValid
    writeBufferValid := false.B
    when(can_retire0 && mem_write0) {
        writeBufferValid := true.B
        writeBufferIndex := idx0
    }.elsewhen(can_retire1 && mem_write1) {
        writeBufferValid := true.B
        writeBufferIndex := idx1
    }.elsewhen(can_retire2 && mem_write2) {
        writeBufferValid := true.B
        writeBufferIndex := idx2
    }.elsewhen(can_retire3 && mem_write3) {
        writeBufferValid := true.B
        writeBufferIndex := idx3
    }

    when(can_retire0) {
        reg_commit_register(0)                       := ROB(idx0).dest
        reg_commit_register_enable(0)                := true.B
        reg_commit_register_old(0)                   := ROB(idx0).dest_old
        reg_commit_register_origin(0)                := ROB(idx0).dest_origin
        ROB(idx0).valid                              := false.B
        ROB(idx0).commited                           := false.B
        branchUpdateQueue.io.in.valid(0)             := ROB(idx0).branch_valid
        branchUpdateQueue.io.in.PC_where_branched(0) := ROB(idx0).PC_where_branched
        branchUpdateQueue.io.in.direction_true(0)    := ROB(idx0).direction_true
        branchUpdateQueue.io.in.target_true(0)       := ROB(idx0).target_true
        branchUpdateQueue.io.in.branch_type(0)       := ROB(idx0).branch_type
    }.otherwise {
        branchUpdateQueue.io.in.valid(0)             := false.B
        branchUpdateQueue.io.in.PC_where_branched(0) := 0.U
        branchUpdateQueue.io.in.direction_true(0)    := false.B
        branchUpdateQueue.io.in.target_true(0)       := 0.U
        branchUpdateQueue.io.in.branch_type(0)       := 0.U
    }

    when(can_retire1) {
        reg_commit_register(1)                       := ROB(idx1).dest
        reg_commit_register_enable(1)                := true.B
        reg_commit_register_old(1)                   := ROB(idx1).dest_old
        reg_commit_register_origin(1)                := ROB(idx1).dest_origin
        ROB(idx1).valid                              := false.B
        ROB(idx1).commited                           := false.B
        branchUpdateQueue.io.in.valid(1)             := ROB(idx1).branch_valid
        branchUpdateQueue.io.in.PC_where_branched(1) := ROB(idx1).PC_where_branched
        branchUpdateQueue.io.in.direction_true(1)    := ROB(idx1).direction_true
        branchUpdateQueue.io.in.target_true(1)       := ROB(idx1).target_true
        branchUpdateQueue.io.in.branch_type(1)       := ROB(idx1).branch_type
    }.otherwise {
        branchUpdateQueue.io.in.valid(1)             := false.B
        branchUpdateQueue.io.in.PC_where_branched(1) := 0.U
        branchUpdateQueue.io.in.direction_true(1)    := false.B
        branchUpdateQueue.io.in.target_true(1)       := 0.U
        branchUpdateQueue.io.in.branch_type(1)       := 0.U
    }

    when(can_retire2) {
        reg_commit_register(2)                       := ROB(idx2).dest
        reg_commit_register_enable(2)                := true.B
        reg_commit_register_old(2)                   := ROB(idx2).dest_old
        reg_commit_register_origin(2)                := ROB(idx2).dest_origin
        ROB(idx2).valid                              := false.B
        ROB(idx2).commited                           := false.B
        branchUpdateQueue.io.in.valid(2)             := ROB(idx2).branch_valid
        branchUpdateQueue.io.in.PC_where_branched(2) := ROB(idx2).PC_where_branched
        branchUpdateQueue.io.in.direction_true(2)    := ROB(idx2).direction_true
        branchUpdateQueue.io.in.target_true(2)       := ROB(idx2).target_true
        branchUpdateQueue.io.in.branch_type(2)       := ROB(idx2).branch_type
    }.otherwise {
        branchUpdateQueue.io.in.valid(2)             := false.B
        branchUpdateQueue.io.in.PC_where_branched(2) := 0.U
        branchUpdateQueue.io.in.direction_true(2)    := false.B
        branchUpdateQueue.io.in.target_true(2)       := 0.U
        branchUpdateQueue.io.in.branch_type(2)       := 0.U
    }

    when(can_retire3) {
        reg_commit_register(3)                       := ROB(idx3).dest
        reg_commit_register_enable(3)                := true.B
        reg_commit_register_old(3)                   := ROB(idx3).dest_old
        reg_commit_register_origin(3)                := ROB(idx3).dest_origin
        ROB(idx3).valid                              := false.B
        ROB(idx3).commited                           := false.B
        branchUpdateQueue.io.in.valid(3)             := ROB(idx3).branch_valid
        branchUpdateQueue.io.in.PC_where_branched(3) := ROB(idx3).PC_where_branched
        branchUpdateQueue.io.in.direction_true(3)    := ROB(idx3).direction_true
        branchUpdateQueue.io.in.target_true(3)       := ROB(idx3).target_true
        branchUpdateQueue.io.in.branch_type(3)       := ROB(idx3).branch_type
    }.otherwise {
        branchUpdateQueue.io.in.valid(3)             := false.B
        branchUpdateQueue.io.in.PC_where_branched(3) := 0.U
        branchUpdateQueue.io.in.direction_true(3)    := false.B
        branchUpdateQueue.io.in.target_true(3)       := 0.U
        branchUpdateQueue.io.in.branch_type(3)       := 0.U
    }

    // 本逻辑的前提：连续四条指令中最多只有一条是UART数据读取指令
    reg_uart_commit := can_retire0 && ROB(idx0).uart_data_read ||
        can_retire1 && ROB(idx1).uart_data_read ||
        can_retire2 && ROB(idx2).uart_data_read ||
        can_retire3 && ROB(idx3).uart_data_read

    // Update head_pointer based on how many instructions were retired
    when(can_retire3 && !reset.asBool && !io.in.clear) {
        head_pointer := (head_pointer + 4.U) % nROBEntries.U
    }.elsewhen(can_retire2) {
        head_pointer := (head_pointer + 3.U) % nROBEntries.U
    }.elsewhen(can_retire1) {
        head_pointer := (head_pointer + 2.U) % nROBEntries.U
    }.elsewhen(can_retire0) {
        head_pointer := (head_pointer + 1.U) % nROBEntries.U
    }.otherwise {
        head_pointer := head_pointer
    }

    when(io.in.clear) {
        recoverMode  := false.B
        lastValidId  := 0.U
        head_pointer := 0.U
        tail_pointer := 0.U
        for (i <- 0 until nROBEntries) {
            ROB(i).valid    := false.B
            ROB(i).commited := false.B
            ROB(i).branch_valid := false.B
        }
    }

    branchUpdateQueue.io.in.clear := false.B // should never be cleared

    // Outputs (connecting the registers to the output ports)
    io.out.commit_register        := reg_commit_register
    io.out.dest_origin            := reg_commit_register_origin
    io.out.dest_old               := reg_commit_register_old
    io.out.commit_register_enable := reg_commit_register_enable
    io.out.head_pointer           := head_pointer
    io.out.tail_pointer           := tail_pointer
    io.out.ROB_full               := ROB_full
    io.out.ROB_empty              := (used_entries === 0.U)
    io.out.writeBufferIndex       := writeBufferIndex
    io.out.writeBufferValid       := writeBufferValid
    io.out.record_indexes         := reg_record_indexes
    io.out.uart_commit            := reg_uart_commit

    io.out.branch_info.valid             := branchUpdateQueue.io.out.valid
    io.out.branch_info.PC_where_branched := branchUpdateQueue.io.out.PC_where_branched
    io.out.branch_info.direction_true    := branchUpdateQueue.io.out.direction_true
    io.out.branch_info.target_true       := branchUpdateQueue.io.out.target_true
    io.out.branch_info.branch_type       := branchUpdateQueue.io.out.branch_type

    // 记录IPC
    if (PERF_STATS) {
        val instCount  = RegInit(0.U(32.W))
        val cycleCount = RegInit(0.U(32.W))
        cycleCount := cycleCount + 1.U
        instCount := instCount + can_retire0.asUInt + can_retire1.asUInt + can_retire2.asUInt + can_retire3.asUInt
        debugSignal(instCount)
        debugSignal(cycleCount)
    }
}

object ReorderBuffer extends App {
    ChiselStage.emitSystemVerilogFile(
      new ReorderBuffer(nROBEntries = 32, nPsyRegs = 64),
      Array("--target-dir", "generated")
    )
}
