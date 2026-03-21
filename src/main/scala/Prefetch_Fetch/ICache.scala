import chisel3._
import chisel3.util._
import CONSTANTS.CONFIG._
import os.stat

class ICacheInterface extends Bundle {
    // CPU侧接口
    val cpu = new Bundle {
        val req_valid  = Input(Bool())
        val req_addr   = Input(UInt(32.W))
        val resp_valid = Output(Bool())
        val resp_data  = Output(Vec(4, UInt(32.W))) // 4条指令
        val stall      = Output(Bool())
    }

    // SRAM侧接口
    val sram = new Bundle {
        val req_valid  = Output(Bool())
        val req_addr   = Output(UInt(32.W))
        val req_we     = Output(Bool())
        val req_data   = Output(UInt(32.W))
        val req_be     = Output(UInt(4.W))
        val resp_valid = Input(Bool())
        val resp_data  = Input(UInt(32.W))
    }
}

class ICache extends Module {
    val io            = IO(new ICacheInterface)
    val iCacheRefresh = IO(Input(Bool())) // iCacheRefresh 不是“刷新 cache”，只是“中止 refill”，命名存在误导

    // 使用默认参数，不进行参数化
    val cacheSize: Int = 8192 // 8KB Cache
    val blockSize: Int = 64   // 64字节块(16条指令)
    val ways: Int = 2         // 2路组相联

    // Cache参数计算
    val blockBytes = blockSize
    val blockWords = blockBytes / 4
    val sets       = cacheSize / (blockSize * ways) // 组数 64
    val offsetBits = log2Ceil(blockBytes)           // 块内字节偏移地址位数 6
    val indexBits  = log2Ceil(sets)                 // 组索引地址位数 6
    val tagBits    = 32 - offsetBits - indexBits    // Way 对外有效地址位 除去低 12 位

    /* ----------------------------- */
    /* -------进入第一个时钟周期------- */
    /* ----------------------------- */

    // 地址解析
    val req_offset      = io.cpu.req_addr(offsetBits - 1, 0)                      // 块内字节偏移地址 6
    val req_index       = io.cpu.req_addr(offsetBits + indexBits - 1, offsetBits) // 组索引地址 6
    val req_tag         = io.cpu.req_addr(31, offsetBits + indexBits)             // 剩余地址
    val req_word_offset = req_offset(offsetBits - 1, 2) // 字偏移

    dontTouch(req_offset)
    dontTouch(req_index)
    dontTouch(req_tag)
    dontTouch(req_word_offset)

    // Cache存储
    val tag_array   = RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(0.U(tagBits.W))))))
    val valid_array = RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(false.B)))))
    val data_array  = SyncReadMem(sets, Vec(ways, Vec(blockWords, UInt(32.W))))
    val lru_array   = RegInit(VecInit(Seq.fill(sets)(0.U(log2Ceil(ways).W))))

    // 读取tag和data
    val tag_read   = tag_array(req_index)
    val valid_read = valid_array(req_index)
    val data_read  = data_array.read(req_index)

    // Hit检测
    val hit     = WireInit(false.B)
    val hit_way = WireInit(8.U(log2Ceil(ways).W))
    for (i <- 0 until ways) {
        when(valid_read(i) && tag_read(i) === req_tag) { // Tag 一致 且 way 有效
            hit     := true.B
            hit_way := i.U
        }
    }

    // 默认输出
    io.cpu.resp_valid := false.B
    io.cpu.resp_data  := VecInit(Seq.fill(4)(0.U(32.W)))
    io.cpu.stall      := false.B
    io.sram.req_valid := false.B
    io.sram.req_addr  := 0.U
    io.sram.req_we    := false.B
    io.sram.req_data  := 0.U
    io.sram.req_be    := 0.U

    // 状态机
    val sIdle :: sMiss :: sFill :: sWrite :: Nil = Enum(4)
    val state                                    = RegInit(sIdle)

    // 集中处理stall信号
    val should_stall = WireInit(false.B)
    switch(state) {
        is(sIdle) {
            when(io.cpu.req_valid && !hit) { // Cache Miss
                should_stall := true.B
            }.otherwise {
                should_stall := false.B
            }
        }
        is(sMiss) {
            should_stall := true.B
        }
        is(sFill) {
            should_stall := true.B
        }
        is(sWrite) {
            should_stall := true.B
        }
    }
    io.cpu.stall := should_stall

    // Miss处理
    val miss_addr    = RegInit(0.U(32.W))
    val fill_counter = RegInit(0.U(log2Ceil(blockWords + 1).W))
    val fill_way     = RegInit(0.U(log2Ceil(ways).W))
    val fill_data    = Reg(Vec(blockWords, UInt(32.W)))
    val old_data     = Reg(Vec(ways, Vec(blockWords, UInt(32.W))))

    switch(state) {
        is(sIdle) {
            // io.cpu.stall := false.B
            when(io.cpu.req_valid) {
                when(hit) { // Cache Hit
                    lru_array(req_index) := ~hit_way // 这个 way hit 了，下次就不替换这个 way，记录信息
                }.elsewhen(!iCacheRefresh) { // Cache Miss
                    state        := sMiss
                    miss_addr    := Cat(req_tag, req_index, 0.U(offsetBits.W))
                    fill_counter := 0.U
                    fill_way     := lru_array(req_index) // 选择 LRU way
                    old_data     := data_array.read(req_index)
                }
            }
        }
        is(sMiss) {
            when(iCacheRefresh) {
                state := sIdle
            }.otherwise { // 向 SRAM 发起读请求，并切换至填充状态
                state             := sFill
                io.sram.req_valid := true.B 
                io.sram.req_addr  := miss_addr + (fill_counter << 2)
                io.sram.req_we    := false.B
                io.sram.req_be    := "b1111".U
                // io.cpu.stall      := true.B
            }
        }
        is(sFill) {
            when(iCacheRefresh) {
                state := sIdle
            }.elsewhen(io.sram.resp_valid) { // 收到了 SRAM 的读请求
                fill_counter := fill_counter + 1.U
                when(fill_counter === blockWords.U) {
                    state := sWrite
                }.otherwise {
                    // 继续填充下一个字
                    fill_data(fill_counter(log2Ceil(blockWords) - 1, 0)) := io.sram.resp_data
                    io.sram.req_valid := true.B
                    io.sram.req_addr  := miss_addr + ((fill_counter + 1.U) << 2)
                    io.sram.req_we    := false.B
                    io.sram.req_be    := "b1111".U
                }
            }.otherwise {
                // 等待SRAM响应
                io.sram.req_valid := true.B
                io.sram.req_addr  := miss_addr + (fill_counter << 2)
                io.sram.req_we    := false.B
                io.sram.req_be    := "b1111".U
                // io.cpu.stall      := true.B
            }
        }
        is(sWrite) {
            // io.cpu.stall := true.B
            // 填充完成，写入Cache
            val write_index = miss_addr(offsetBits + indexBits - 1, offsetBits)

            // 写入数据
            val new_data = VecInit(Seq.tabulate(ways) { w =>
                Mux(w.U === fill_way, fill_data, old_data(w))
            })
            data_array.write(write_index, new_data)

            // 写入tag
            val new_tags = VecInit(Seq.tabulate(ways) { w =>
                Mux(w.U === fill_way, miss_addr(31, offsetBits + indexBits), tag_read(w))
            })
            // tag_array.write(write_index, new_tags)
            tag_array(write_index) := new_tags

            // 设置valid
            valid_array(write_index)(fill_way) := true.B
            state                              := sIdle
        }
    }

    /* ----------------------------- */
    /* -------进入第二个时钟周期------- */
    /* ----------------------------- */

    val reg_hit          = RegNext(hit)
    val reg_hit_way      = RegNext(hit_way)
    val reg_should_stall = RegNext(should_stall)
    val reg_word_offset  = RegNext(req_word_offset)
    val reg_index        = RegNext(req_index) // 需要添加这个寄存器

    when(reg_hit && !reg_should_stall) {
        val hit_data = data_read(reg_hit_way) // 注意：data_read 在本时钟周期才得到
        val base_word_offset = reg_word_offset & ~3.U(reg_word_offset.getWidth.W) // 把 word_offset 的低2位清零
        for (i <- 0 until 4) {
            io.cpu.resp_data(i) := hit_data(base_word_offset + i.U)
        }
        io.cpu.resp_valid := true.B
        // 不需要更新，上一时钟周期已经完成: lru_array(reg_index) := reg_hit_way // 更新LRU : 在相应地址存储这一次命中的 cache way
    }.otherwise {
        io.cpu.resp_valid := false.B
        io.cpu.resp_data  := VecInit(Seq.fill(4)(0.U(32.W)))
    }

    if (PERF_STATS) {
        val memRequests = RegInit(0.U(32.W)) // 统计内存请求次数
        val cacheHits   = RegInit(0.U(32.W)) // 统计Cache命中次数

        when(!RegNext(io.cpu.stall, false.B)) {
            memRequests := memRequests + 1.U
            when(hit) {
                cacheHits := cacheHits + 1.U
            }
        }
        debugSignal(memRequests)
        debugSignal(cacheHits)
    }
}
