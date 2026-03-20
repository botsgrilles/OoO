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

class ICache(
  cacheSize: Int = 8192, // 8KB Cache
  blockSize: Int = 64,   // 64字节块(16条指令)
  ways: Int = 2          // 2路组相联
) extends Module {
    val io            = IO(new ICacheInterface)
    val iCacheRefresh = IO(Input(Bool()))

    // Cache参数计算
    val blockBytes = blockSize
    val blockWords = blockBytes / 4
    val sets       = cacheSize / (blockSize * ways) // 组数
    val offsetBits = log2Ceil(blockBytes)           // 块内字节偏移
    val indexBits  = log2Ceil(sets)                 // 组索引
    val tagBits    = 32 - offsetBits - indexBits

    // 地址解析
    val req_offset      = io.cpu.req_addr(offsetBits - 1, 0)
    val req_index       = io.cpu.req_addr(offsetBits + indexBits - 1, offsetBits)
    val req_tag         = io.cpu.req_addr(31, offsetBits + indexBits)
    val req_word_offset = req_offset(offsetBits - 1, 2) // 字偏移

    debugSignal(req_offset)
    debugSignal(req_index)
    debugSignal(req_tag)
    debugSignal(req_word_offset)

    // Cache存储
    // val tag_array = SyncReadMem(sets, Vec(ways, UInt(tagBits.W)))
    val tag_array = RegInit(
      VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(0.U(tagBits.W)))))
    )
    val valid_array = RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(false.B)))))
    val data_array  = SyncReadMem(sets, Vec(ways, Vec(blockWords, UInt(32.W))))
    val lru_array   = RegInit(VecInit(Seq.fill(sets)(0.U(log2Ceil(ways).W))))

    // 状态机
    val sIdle :: sMiss :: sFill :: sWrite :: Nil = Enum(4)
    val state                                    = RegInit(sIdle)

    // Miss处理
    val miss_addr    = RegInit(0.U(32.W))
    val fill_counter = RegInit(0.U(log2Ceil(blockWords + 1).W))
    val fill_way     = RegInit(0.U(log2Ceil(ways).W))
    val fill_data    = Reg(Vec(blockWords, UInt(32.W)))
    val old_data     = Reg(Vec(ways, Vec(blockWords, UInt(32.W))))

    // 读取tag和data
    val tag_read   = tag_array(req_index)
    val data_read  = data_array.read(req_index)
    val valid_read = valid_array(req_index)

    // Hit检测
    val hit_way = Wire(UInt(log2Ceil(ways).W))
    val hit     = Wire(Bool())

    val reg_hit_way     = RegInit(0.U(log2Ceil(ways).W))
    val reg_hit         = RegInit(false.B)
    val reg_word_offset = RegInit(0.U(log2Ceil(blockWords).W))

    hit     := false.B
    hit_way := 0.U

    for (i <- 0 until ways) {
        when(valid_read(i) && tag_read(i) === req_tag) {
            hit     := true.B
            hit_way := i.U
        }
    }

    reg_hit         := hit
    reg_hit_way     := hit_way
    reg_word_offset := req_word_offset

    // 默认输出
    io.cpu.resp_valid := false.B
    io.cpu.resp_data  := VecInit(Seq.fill(4)(0.U(32.W)))
    io.cpu.stall      := false.B
    io.sram.req_valid := false.B
    io.sram.req_addr  := 0.U
    io.sram.req_we    := false.B
    io.sram.req_data  := 0.U
    io.sram.req_be    := 0.U

    // 集中处理stall信号
    val should_stall     = Wire(Bool())
    val reg_should_stall = RegInit(false.B)
    should_stall     := false.B
    reg_should_stall := should_stall

    switch(state) {
        is(sIdle) {
            when(io.cpu.req_valid && !hit) {
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
            // should_stall := false.B
        }
    }

    io.cpu.stall := should_stall

    when(reg_hit && !reg_should_stall) {
        val hit_data  = data_read(reg_hit_way)
        val base_word = reg_word_offset & ~3.U(log2Ceil(blockWords).W)
        for (i <- 0 until 4) {
            val word_idx = (base_word + i.U) % blockWords.U
            io.cpu.resp_data(i) := hit_data(word_idx)
        }
        io.cpu.resp_valid := true.B

        // 更新LRU
        val reg_index = RegInit(0.U(indexBits.W)) // 需要添加这个寄存器
        lru_array(reg_index) := reg_hit_way
    }.otherwise {
        io.cpu.resp_valid := false.B
        io.cpu.resp_data  := VecInit(Seq.fill(4)(0.U(32.W)))
    }

    switch(state) {
        is(sIdle) {
            // io.cpu.stall := false.B
            when(io.cpu.req_valid) {
                when(hit) {
                    // Cache命中
                    // data_read(hit_way)
                    // // 根据字对齐地址返回4条连续指令
                    // val base_word = req_word_offset & ~3.U(log2Ceil(blockWords).W)
                    // for (i <- 0 until 4) {
                    //     val word_idx = (base_word + i.U) % blockWords.U
                    //     io.cpu.resp_data(i) := hit_data(word_idx)
                    // }
                    // io.cpu.resp_valid := true.B

                    // 更新LRU
                    lru_array(req_index) := hit_way
                }.elsewhen(!iCacheRefresh) {
                    // Cache缺失
                    state        := sMiss
                    miss_addr    := Cat(req_tag, req_index, 0.U(offsetBits.W))
                    fill_counter := 0.U
                    fill_way     := lru_array(req_index) // 选择LRU way
                    old_data     := data_array.read(req_index)
                    // io.cpu.stall := true.B
                }
            }
        }
        is(sMiss) {
            // 开始填充
            when(iCacheRefresh) {
                state := sIdle
            }.otherwise {
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
                state := sIdle // Abort fill on refresh
            }.elsewhen(io.sram.resp_valid) {
                // io.cpu.stall            := true.B
                fill_counter            := fill_counter + 1.U

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
