import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import chisel3.experimental._
import CONSTANTS.RAM_MAP._
import CONSTANTS.CONFIG._
import CONSTANTS.EXECUTION_CYCLE

class MySRAMInterface extends Bundle {
    // 对外部SRAM的接口
    val data_in     = Input(UInt(32.W))
    val data_out    = Output(UInt(32.W))
    val data_out_en = Output(Bool())
    val addr        = Output(UInt(log2Ceil(DataRAMSize / 4).W))
    val be_n        = Output(UInt(4.W))
    val ce_n        = Output(Bool())
    val oe_n        = Output(Bool())
    val we_n        = Output(Bool())
}

class UARTInterface extends Bundle {
    val TxDReady = Input(Bool())
    val TxDValid = Output(Bool())
    val TxDData  = Output(UInt(8.W))

    val RxdReady = Output(Bool())
    val RxdValid = Input(Bool())
    val RxdData  = Input(UInt(8.W))
}

/*
    内存控制器 - 支持并行访问
    优先级仲裁：iCache读 > WriteBuffer读 > WriteBuffer写
    不同设备可以并行访问（BaseRAM, ExtRAM, UART可以同时进行）
 */
class SRAMController extends Module {
    val io = IO(new Bundle {
        // iCache接口 (只读)
        val icache_req = new Bundle {
            val valid = Input(Bool())
            val addr  = Input(UInt(32.W))
        }
        val icache_resp = new Bundle {
            val valid = Output(Bool())
            val data  = Output(UInt(32.W))
        }

        // WriteBuffer接口 (读写)
        val wb_req = new Bundle {
            val valid = Input(Bool())
            val addr  = Input(UInt(32.W))
            val we    = Input(Bool())
            val data  = Input(UInt(32.W))
            val be    = Input(UInt(4.W))
        }
        val wb_resp = new Bundle {
            val valid          = Output(Bool())
            val data           = Output(UInt(32.W))
            val uart_data_read = Output(Bool())
        }

        // SRAM物理接口
        val base_ram = new MySRAMInterface
        val ext_ram  = new MySRAMInterface
        val uart     = new UARTInterface
    })

    // 处理ExtRAM字节序
    def reverseBytes32(x: UInt): UInt = Cat(x(7, 0), x(15, 8), x(23, 16), x(31, 24))

    // 地址映射函数
    /*
    def getDevice(addr: UInt, req_valid: Bool): UInt = {
        Mux(req_valid, MuxCase(
          dev_INVALID,
          Seq(
            ((addr >= BASE_RAM_START) && (addr <= BASE_RAM_END)) -> dev_BASE,
            ((addr >= EXT_RAM_START) && (addr <= EXT_RAM_END))   -> dev_EXT,
            ((addr === UART_DATA) || (addr === UART_FLAGS))     -> dev_UART
          )
        ), dev_NONE)
    }
     */

    def getDevice(addr: UInt, req_valid: Bool): UInt = {
        Mux(
          req_valid,
          MuxCase(
            dev_INVALID,
            Seq(
              (addr(31, 22) === BASE_RAM_START(31, 22)) -> dev_BASE,
              (addr(31, 22) === EXT_RAM_START(31, 22))  -> dev_EXT,
              (addr(31, 22) === UART_DATA(31, 22))      -> dev_UART
            )
          ),
          dev_NONE
        )
    }

    def map_addr(addr: UInt, start: UInt): UInt = {
        (addr - start)(log2Ceil(DataRAMSize), 2)
    }

    def mask_with_be(data: UInt, be: UInt): UInt = {
        Cat(
          Mux(be(3), data(31, 24), 0.U(8.W)),
          Mux(be(2), data(23, 16), 0.U(8.W)),
          Mux(be(1), data(15, 8), 0.U(8.W)),
          Mux(be(0), data(7, 0), 0.U(8.W))
        )
    }

    // 请求类型枚举
    // val req_NONE :: req_ICACHE :: req_WB_READ :: req_WB_WRITE :: Nil = Enum(4)
    val req_NONE = "b0001".U(4.W)
    val req_ICACHE = "b0010".U(4.W)
    val req_WB_READ = "b0100".U(4.W)
    val req_WB_WRITE = "b1000".U(4.W)

    // 设备类型
    // val dev_BASE :: dev_EXT :: dev_UART :: dev_INVALID :: dev_NONE :: Nil = Enum(5)
    val dev_BASE    = "b00001".U(5.W)
    val dev_EXT     = "b00010".U(5.W)
    val dev_UART    = "b00100".U(5.W)
    val dev_INVALID = "b01000".U(5.W)
    val dev_NONE    = "b10000".U(5.W)

    // 请求分析
    val icache_device = getDevice(io.icache_req.addr, io.icache_req.valid)
    val wb_device     = getDevice(io.wb_req.addr, io.wb_req.valid)

    val icache_device_reg = RegInit(dev_NONE)
    val wb_device_reg     = RegInit(dev_NONE)

    icache_device_reg := icache_device

    wb_device_reg := wb_device

    icache_device_reg := icache_device
    wb_device_reg     := wb_device

    debugSignal(icache_device)
    debugSignal(wb_device)

    val sIdle :: sBusy :: Nil = Enum(2)

    // BaseRAM状态机
    val base_state        = RegInit(sIdle)
    val base_delay_reg    = RegInit(0.U(log2Ceil(EXECUTION_CYCLE.MEM).W))
    val base_req_type_reg = RegInit(req_NONE)
    val base_addr_reg     = RegInit(0.U(32.W))
    val base_data_reg     = RegInit(0.U(32.W))
    val base_be_reg       = RegInit(0.U(4.W))
    val base_is_write_reg = RegInit(false.B)

    // ExtRAM状态机
    val ext_state        = RegInit(sIdle)
    val ext_delay_reg    = RegInit(0.U(log2Ceil(EXECUTION_CYCLE.MEM).W))
    val ext_req_type_reg = RegInit(req_NONE)
    val ext_addr_reg     = RegInit(0.U(32.W))
    val ext_data_reg     = RegInit(0.U(32.W))
    val ext_be_reg       = RegInit(0.U(4.W))
    val ext_is_write_reg = RegInit(false.B)

    // UART状态机
    val uart_state = RegInit(sIdle)
    // val uart_delay            = RegInit(0.U(2.W))
    val uart_addr_reg         = RegInit(0.U(32.W))
    val uart_read_data_reg    = RegInit(0.U(32.W))
    val uart_is_read_data_reg = RegInit(false.B)
    val uart_is_write_reg     = RegInit(false.B)
    val uart_write_data_reg   = RegInit(0.U(8.W))

    val uart_read_data = Cat(0.U(24.W), io.uart.RxdData)
    val uart_read_flags = Cat(
      0.U(30.W),
      io.uart.RxdValid,
      io.uart.TxDReady && !uart_is_write_reg
    ) // 防止写入当周期flag未更新

    // 仲裁逻辑：为每个设备独立仲裁
    val base_new_req      = Wire(Bool())
    val ext_new_req       = Wire(Bool())
    val uart_new_req      = Wire(Bool())
    val base_new_req_type = Wire(UInt(4.W))
    val ext_new_req_type  = Wire(UInt(4.W))
    val uart_new_req_type = Wire(UInt(4.W))

    debugSignal(uart_new_req)
    debugSignal(uart_new_req_type)

    io.icache_resp.valid      := false.B
    io.icache_resp.data       := 0.U
    io.wb_resp.valid          := false.B
    io.wb_resp.data           := 0.U
    io.wb_resp.uart_data_read := false.B

    io.uart.TxDData  := 0.U
    io.uart.TxDValid := false.B
    io.uart.RxdReady := false.B

    debugSignal(io.wb_resp.data)

    // BaseRAM仲裁
    base_new_req      := false.B
    base_new_req_type := req_NONE
    when(
      io.icache_req.valid && icache_device(0) &&
          (base_state === sIdle || (base_state === sBusy && base_delay_reg === (EXECUTION_CYCLE.MEM - 1).U))
    ) {
        base_new_req      := true.B
        base_new_req_type := req_ICACHE
    }.elsewhen(
      io.wb_req.valid && wb_device(0) &&
          (base_state === sIdle || (base_state === sBusy && base_delay_reg === (EXECUTION_CYCLE.MEM - 1).U))
    ) {
        base_new_req      := true.B
        base_new_req_type := Mux(io.wb_req.we, req_WB_WRITE, req_WB_READ)
    }

    // ExtRAM仲裁
    ext_new_req      := false.B
    ext_new_req_type := req_NONE
    when(
      io.icache_req.valid && icache_device(1) &&
          (ext_state === sIdle || (ext_state === sBusy && ext_delay_reg === (EXECUTION_CYCLE.MEM - 1).U))
    ) {
        ext_new_req      := true.B
        ext_new_req_type := req_ICACHE
    }.elsewhen(
      io.wb_req.valid && wb_device(1) &&
          (ext_state === sIdle || (ext_state === sBusy && ext_delay_reg === (EXECUTION_CYCLE.MEM - 1).U))
    ) {
        ext_new_req      := true.B
        ext_new_req_type := Mux(io.wb_req.we, req_WB_WRITE, req_WB_READ)
    }

    // UART仲裁
    uart_new_req      := false.B
    uart_new_req_type := req_NONE
    when(
      io.icache_req.valid && icache_device(2)
      //   && (uart_state === sIdle || (uart_state === sBusy && uart_delay === (EXECUTION_CYCLE.MEM - 2).U))
    ) {
        uart_new_req      := true.B
        uart_new_req_type := req_ICACHE
    }.elsewhen(
      io.wb_req.valid && wb_device(2)
      //   && (uart_state === sIdle || (uart_state === sBusy && uart_delay === (EXECUTION_CYCLE.MEM - 2).U))
    ) {
        uart_new_req      := true.B
        uart_new_req_type := Mux(io.wb_req.we, req_WB_WRITE, req_WB_READ)
    }

    // 响应合并逻辑
    val icache_resp_valid = Wire(Bool())
    val icache_resp_data  = Wire(UInt(32.W))
    val wb_resp_valid     = Wire(Bool())
    val wb_resp_data      = Wire(UInt(32.W))

    icache_resp_valid := false.B
    icache_resp_data  := 0.U
    wb_resp_valid     := false.B
    wb_resp_data      := 0.U

    // 默认：都置为无效
    io.base_ram.ce_n        := true.B
    io.base_ram.we_n        := true.B
    io.base_ram.oe_n        := true.B
    io.base_ram.be_n        := "b1111".U // 默认字节使能为全1
    io.base_ram.addr        := 0.U
    io.base_ram.data_out    := 0.U
    io.base_ram.data_out_en := false.B

    io.ext_ram.ce_n        := true.B
    io.ext_ram.we_n        := true.B
    io.ext_ram.oe_n        := true.B
    io.ext_ram.be_n        := "b1111".U // 默认字节使能为全1
    io.ext_ram.addr        := 0.U
    io.ext_ram.data_out    := 0.U
    io.ext_ram.data_out_en := false.B

    // BaseRAM状态机
    switch(base_state) {
        is(sIdle) {
            when(base_new_req) {
                base_state        := sBusy
                base_delay_reg    := 0.U
                base_req_type_reg := base_new_req_type
                base_is_write_reg := base_new_req_type(3)

                io.base_ram.ce_n := false.B
                io.base_ram.we_n := ~base_new_req_type(3)
                io.base_ram.oe_n := base_new_req_type(3)
                io.base_ram.be_n := Mux(
                  base_new_req_type(1),
                  "b1111".U,
                  ~io.wb_req.be
                )
                io.base_ram.data_out_en := base_new_req_type(3)

                when(base_new_req_type(1)) {
                    base_addr_reg    := map_addr(io.icache_req.addr, BASE_RAM_START)
                    io.base_ram.addr := map_addr(io.icache_req.addr, BASE_RAM_START)
                    base_data_reg    := 0.U
                    io.base_ram.be_n := "b0000".U // iCache总是全字节访问
                    base_be_reg      := "b1111".U
                }.otherwise {
                    base_addr_reg        := map_addr(io.wb_req.addr, BASE_RAM_START)
                    io.base_ram.addr     := map_addr(io.wb_req.addr, BASE_RAM_START)
                    base_data_reg        := io.wb_req.data
                    io.base_ram.be_n     := ~io.wb_req.be
                    base_be_reg          := io.wb_req.be
                    io.base_ram.data_out := io.wb_req.data
                }
            }
        }
        is(sBusy) {
            io.base_ram.ce_n        := false.B
            io.base_ram.oe_n        := base_is_write_reg
            io.base_ram.addr        := base_addr_reg
            io.base_ram.data_out    := base_data_reg
            io.base_ram.data_out_en := base_is_write_reg
            io.base_ram.be_n        := ~base_be_reg
            io.base_ram.we_n        := !base_is_write_reg

            when(base_delay_reg === (EXECUTION_CYCLE.MEM - 2).U) {
                io.base_ram.we_n := true.B
            }

            when(base_delay_reg === (EXECUTION_CYCLE.MEM - 1).U) {
                // 操作完成，发送响应
                when(base_req_type_reg(1)) {
                    io.icache_resp.valid := true.B
                    io.icache_resp.data  := io.base_ram.data_in
                }.otherwise {
                    io.wb_resp.valid := true.B
                    io.wb_resp.data  := mask_with_be(io.base_ram.data_in, base_be_reg)
                }

                when(base_new_req) {
                    base_state        := sBusy
                    base_delay_reg    := 0.U
                    base_req_type_reg := base_new_req_type
                    base_is_write_reg := base_new_req_type(3)

                    io.base_ram.ce_n        := false.B
                    io.base_ram.we_n        := ~base_new_req_type(3)
                    io.base_ram.oe_n        := base_new_req_type(3)
                    io.base_ram.data_out_en := base_new_req_type(3)

                    when(base_new_req_type(1)) {
                        base_addr_reg    := map_addr(io.icache_req.addr, BASE_RAM_START)
                        io.base_ram.addr := map_addr(io.icache_req.addr, BASE_RAM_START)
                        base_data_reg    := 0.U
                        io.base_ram.be_n := "b0000".U // iCache总是全字节访问
                        base_be_reg      := "b1111".U
                    }.otherwise {
                        base_addr_reg        := map_addr(io.wb_req.addr, BASE_RAM_START)
                        io.base_ram.addr     := map_addr(io.wb_req.addr, BASE_RAM_START)
                        base_data_reg        := io.wb_req.data
                        io.base_ram.be_n     := ~io.wb_req.be
                        base_be_reg          := io.wb_req.be
                        io.base_ram.data_out := io.wb_req.data
                    }
                }.otherwise {
                    base_state := sIdle
                }
            }.otherwise {
                base_delay_reg := base_delay_reg + 1.U
            }
        }
    }

    // ExtRAM状态机
    switch(ext_state) {
        is(sIdle) {
            when(ext_new_req) {
                ext_state        := sBusy
                ext_delay_reg    := 0.U
                ext_req_type_reg := ext_new_req_type
                ext_is_write_reg := ext_new_req_type(3)

                io.ext_ram.ce_n        := false.B
                io.ext_ram.we_n        := ~ext_new_req_type(3)
                io.ext_ram.oe_n        := ext_new_req_type(3)
                io.ext_ram.be_n        := ~io.wb_req.be
                io.ext_ram.data_out_en := ext_new_req_type(3)

                when(ext_new_req_type(1)) {
                    ext_addr_reg        := map_addr(io.icache_req.addr, EXT_RAM_START)
                    io.ext_ram.addr     := map_addr(io.icache_req.addr, EXT_RAM_START)
                    ext_data_reg        := 0.U
                    io.ext_ram.data_out := 0.U
                    ext_be_reg          := "b1111".U
                    io.ext_ram.be_n     := "b0000".U // iCache总是全字节访问
                }.otherwise {
                    ext_addr_reg        := map_addr(io.wb_req.addr, EXT_RAM_START)
                    io.ext_ram.addr     := map_addr(io.wb_req.addr, EXT_RAM_START)
                    ext_data_reg        := io.wb_req.data
                    io.ext_ram.data_out := io.wb_req.data
                    ext_be_reg          := io.wb_req.be
                    io.ext_ram.be_n     := ~io.wb_req.be
                }
            }
        }
        is(sBusy) {
            io.ext_ram.ce_n        := false.B
            io.ext_ram.oe_n        := ext_is_write_reg
            io.ext_ram.be_n        := ~ext_be_reg
            io.ext_ram.addr        := ext_addr_reg
            io.ext_ram.data_out    := ext_data_reg
            io.ext_ram.data_out_en := ext_is_write_reg

            when(ext_delay_reg === (EXECUTION_CYCLE.MEM - 2).U) {
                io.ext_ram.we_n := true.B
            }

            when(ext_delay_reg === (EXECUTION_CYCLE.MEM - 1).U) {
                when(ext_req_type_reg(1)) {
                    io.icache_resp.valid := true.B
                    io.icache_resp.data  := io.ext_ram.data_in
                }.otherwise {
                    io.wb_resp.valid := true.B
                    io.wb_resp.data  := mask_with_be(io.ext_ram.data_in, ext_be_reg)
                }

                when(ext_new_req) {
                    ext_delay_reg    := 0.U
                    ext_req_type_reg := ext_new_req_type
                    ext_is_write_reg := ext_new_req_type(3)

                    io.ext_ram.ce_n        := false.B
                    io.ext_ram.we_n        := ~ext_new_req_type(3)
                    io.ext_ram.oe_n        := ext_new_req_type(3)
                    io.ext_ram.be_n        := ~io.wb_req.be
                    io.ext_ram.data_out_en := ext_new_req_type(3)

                    when(ext_new_req_type(1)) {
                        ext_addr_reg        := map_addr(io.icache_req.addr, EXT_RAM_START)
                        io.ext_ram.addr     := map_addr(io.icache_req.addr, EXT_RAM_START)
                        ext_data_reg        := 0.U
                        io.ext_ram.data_out := 0.U
                        ext_be_reg          := "b1111".U
                        io.ext_ram.be_n     := "b0000".U // iCache总是全字节访问
                    }.otherwise {
                        ext_addr_reg        := map_addr(io.wb_req.addr, EXT_RAM_START)
                        io.ext_ram.addr     := map_addr(io.wb_req.addr, EXT_RAM_START)
                        ext_data_reg        := io.wb_req.data
                        io.ext_ram.data_out := io.wb_req.data
                        ext_be_reg          := io.wb_req.be
                        io.ext_ram.be_n     := ~io.wb_req.be
                    }
                }.otherwise {
                    ext_state := sIdle
                }
            }.otherwise {
                ext_delay_reg := ext_delay_reg + 1.U
            }
        }
    }

    debugSignal(io.uart)
    debugSignal(uart_read_data_reg)

    // UART状态机
    switch(uart_state) {
        is(sIdle) {
            when(uart_new_req) {
                val uart_is_read_data = // 0xbfd003f8 vs 0xbfd003fc
                    uart_new_req_type(2) && io.wb_req.addr(2) === UART_DATA(
                      2
                    )
                uart_state            := sBusy
                uart_is_read_data_reg := uart_is_read_data
                // uart_delay            := 0.U
                uart_is_write_reg := uart_new_req_type(3)
                uart_read_data_reg := Mux(
                  uart_is_read_data,
                  Mux(io.uart.RxdValid, uart_read_data, 0.U),
                  uart_read_flags
                )
                uart_write_data_reg := Mux(
                  uart_new_req_type(3),
                  io.wb_req.data(7, 0),
                  0.U
                )
            }
        }
        is(sBusy) {
            io.uart.TxDValid := uart_is_write_reg
            io.uart.TxDData  := Mux(uart_is_write_reg, uart_write_data_reg(7, 0), 0.U)
            io.uart.RxdReady := uart_is_read_data_reg // 实际上是deque信号
            // when(uart_delay === (EXECUTION_CYCLE.MEM - 2).U) {
            // 操作完成，发送响应K
            io.wb_resp.valid := true.B
            when(!uart_is_write_reg) { // uart is read
                io.wb_resp.data           := uart_read_data_reg
                io.wb_resp.uart_data_read := uart_is_read_data_reg
            }
            when(uart_new_req) {
                // uart_delay    := 0.U
                uart_is_write_reg := uart_new_req_type(3)
                // val uart_is_read_data =
                //     (uart_new_req_type === req_WB_READ && io.wb_req.addr === UART_DATA ||
                // uart_new_req_type === req_ICACHE && io.icache_req.addr === UART_DATA)
                val uart_is_read_data = // 0xbfd003f8 vs 0xbfd003fc
                    uart_new_req_type(2) && io.wb_req.addr(2) === UART_DATA(
                      2
                    )
                uart_is_read_data_reg := uart_is_read_data
                uart_read_data_reg := Mux(
                  uart_is_read_data && io.uart.RxdValid,
                  uart_read_data,
                  uart_read_flags
                )
                uart_write_data_reg := Mux(
                  uart_new_req_type(3),
                  io.wb_req.data(7, 0),
                  0.U
                )
            }.otherwise {
                uart_state := sIdle
            }
            // }.otherwise {
            //     uart_delay := uart_delay + 1.U
            // }
        }
    }

    // iCache优先级更高，如果iCache要访问同一设备，WriteBuffer需要等待
    val icache_conflict = WireInit(false.B)
    when(io.icache_req.valid && io.wb_req.valid) {
        icache_conflict := (icache_device & wb_device).orR
    }

    // 如果地址非法
    when(icache_device_reg(3)) {
        io.icache_resp.valid := true.B
        io.icache_resp.data  := 0.U
    }

    when(wb_device_reg(3)) {
        io.wb_resp.valid := true.B
        io.wb_resp.data  := 0.U
    }
}
