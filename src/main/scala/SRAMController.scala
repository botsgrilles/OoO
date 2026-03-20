import chisel3._
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

class SRAMController extends Module {
    val io = IO(new Bundle {
        // CPU侧请求接口
        val icache_req = new Bundle {
            val valid = Input(Bool())
            val addr  = Input(UInt(32.W))
            val we    = Input(Bool())
            val data  = Input(UInt(32.W))
            val be    = Input(UInt(4.W))
        }
        val icache_resp = new Bundle {
            val valid = Output(Bool())
            val data  = Output(UInt(32.W))
        }

        val dcache_req = new Bundle {
            val valid = Input(Bool())
            val addr  = Input(UInt(32.W))
            val we    = Input(Bool())
            val data  = Input(UInt(32.W))
            val be    = Input(UInt(4.W))
        }
        val dcache_resp = new Bundle {
            val valid          = Output(Bool())
            val data           = Output(UInt(32.W))
            val busy           = Output(Bool())
            val uart_data_read = Output(Bool())
        }

        // SRAM物理接口
        val base_ram = new MySRAMInterface
        val ext_ram  = new MySRAMInterface
        val uart     = new UARTInterface
    })

    val uart_status = Cat(0.U(30.W), io.uart.RxdReady, io.uart.TxDReady)

    // 处理ExtRAM字节序
    def reverseBytes32(x: UInt): UInt = Cat(x(7, 0), x(15, 8), x(23, 16), x(31, 24))

    val dcache_data =
        if (ExtRAMReverseOrder) reverseBytes32(io.dcache_req.data) else io.dcache_req.data

    io.base_ram.data_out := 0.U
    io.ext_ram.data_out  := 0.U

    // 地址映射：指令用base_ram，数据用ext_ram
    val icache_use_base =
        io.icache_req.addr >= BASE_RAM_START && io.icache_req.addr < BASE_RAM_END
    val dcache_use_ext =
        io.dcache_req.addr >= EXT_RAM_START && io.dcache_req.addr < EXT_RAM_END
    val dcache_use_uart_data  = io.dcache_req.addr === UART_DATA
    val dcache_use_uart_flags = io.dcache_req.addr === UART_FLAGS

    io.base_ram.addr := 0.U
    io.base_ram.be_n := "b0000".U
    io.base_ram.ce_n := true.B
    io.base_ram.oe_n := true.B
    io.base_ram.we_n := true.B

    io.ext_ram.addr := 0.U
    io.ext_ram.be_n := "b1111".U
    io.ext_ram.ce_n := true.B
    io.ext_ram.oe_n := true.B
    io.ext_ram.we_n := true.B

    // 控制数据输出使能：当ce_n和we_n都为低时，控制器驱动数据总线
    io.base_ram.data_out_en := false.B
    io.ext_ram.data_out_en  := false.B

    val uart_read_data_reg   = Reg(UInt(32.W))
    val uart_read_flags_reg  = Reg(UInt(32.W))
    val uart_read_data_reg2  = Reg(UInt(32.W))
    val uart_read_flags_reg2 = Reg(UInt(32.W))

    uart_read_data_reg   := Cat(0.U(24.W), io.uart.RxdData)
    uart_read_flags_reg  := Cat(0.U(30.W), io.uart.RxdReady, io.uart.TxDReady)
    uart_read_data_reg2  := uart_read_data_reg
    uart_read_flags_reg2 := uart_read_flags_reg

    // 状态机
    val base_sIdle :: base_sRead :: base_sWrite :: Nil = Enum(3)
    val ext_sIdle :: ext_sRead :: ext_sWrite :: Nil    = Enum(3)
    val uart_sIdle :: uart_sRead :: uart_sWrite :: Nil = Enum(3)

    val base_state         = RegInit(base_sIdle)
    val ext_state          = RegInit(ext_sIdle)
    val uart_state         = RegInit(uart_sIdle)
    val base_addr          = RegInit(0.U(32.W))
    val ext_addr           = RegInit(0.U(32.W))
    val base_data          = RegInit(0.U(32.W))
    val ext_data           = RegInit(0.U(32.W))
    val base_be            = RegInit("b0000".U(4.W))
    val ext_be             = RegInit("b1111".U(4.W))
    val base_delay_counter = RegInit(0.U(2.W))
    val ext_delay_counter  = RegInit(0.U(2.W))
    val uart_delay_counter = RegInit(0.U(2.W))

    io.icache_resp.valid          := false.B
    io.icache_resp.data           := 0.U
    io.dcache_resp.valid          := false.B
    io.dcache_resp.data           := 0.U
    io.dcache_resp.uart_data_read := false.B

    // UART 接口逻辑
    io.uart.TxDValid := false.B
    io.uart.TxDData  := 0.U
    io.uart.RxdReady := false.B

    val baseReadData = io.base_ram.data_in
    val extReadData  = io.ext_ram.data_in

    // 仲裁信号
    val ext_read_fire        = WireDefault(false.B)
    val uart_data_read_fire  = WireDefault(false.B)
    val uart_flags_read_fire = WireDefault(false.B)

    /* BaseRAM 状态机 */
    switch(base_state) {
        is(base_sIdle) {
            when(io.icache_req.valid && icache_use_base) {
                io.base_ram.addr :=
                    (io.icache_req.addr - BASE_RAM_START)(log2Ceil(DataRAMSize), 2)
                io.base_ram.ce_n := false.B
                io.base_ram.be_n := ~io.icache_req.be
                base_addr :=
                    (io.icache_req.addr - BASE_RAM_START)(log2Ceil(DataRAMSize), 2)
                base_be   := ~io.icache_req.be
                base_data := io.icache_req.data
                when(io.icache_req.we) {
                    base_state              := base_sWrite
                    io.base_ram.we_n        := false.B
                    io.base_ram.oe_n        := true.B
                    io.base_ram.data_out    := io.icache_req.data
                    io.base_ram.data_out_en := true.B
                }.otherwise {
                    base_state       := base_sRead
                    io.base_ram.we_n := true.B
                    io.base_ram.oe_n := false.B
                }
                base_delay_counter := 0.U
            }
        }
        is(base_sRead) {
            // 保持地址和控制信号稳定
            io.base_ram.ce_n := false.B
            io.base_ram.we_n := true.B
            io.base_ram.oe_n := false.B
            io.base_ram.addr := base_addr
            when(base_delay_counter === (EXECUTION_CYCLE.MEM - 2).U) {
                io.icache_resp.valid := true.B
                io.icache_resp.data  := baseReadData
                base_state           := base_sIdle
            }.otherwise {
                base_delay_counter := base_delay_counter + 1.U
            }
        }
        is(base_sWrite) {
            // 保持地址和控制信号稳定
            io.base_ram.ce_n        := false.B
            io.base_ram.we_n        := false.B
            io.base_ram.be_n        := base_be
            io.base_ram.oe_n        := true.B
            io.base_ram.data_out_en := true.B
            io.base_ram.addr        := base_addr
            io.base_ram.data_out    := base_data
            when(base_delay_counter === (EXECUTION_CYCLE.MEM - 2).U) {
                base_state := base_sIdle
            }.otherwise {
                base_delay_counter := base_delay_counter + 1.U
            }
        }
    }

    /* ExtRAM 状态机 */
    switch(ext_state) {
        is(ext_sIdle) {
            when(io.dcache_req.valid && dcache_use_ext) {
                io.ext_ram.addr :=
                    (io.dcache_req.addr - EXT_RAM_START)(log2Ceil(DataRAMSize), 2)
                io.ext_ram.ce_n := false.B
                io.ext_ram.be_n := ~io.dcache_req.be
                ext_addr := (io.dcache_req.addr - EXT_RAM_START)(log2Ceil(DataRAMSize), 2)
                ext_be   := ~io.dcache_req.be
                ext_data := dcache_data
                when(io.dcache_req.we) {
                    ext_state              := ext_sWrite
                    io.ext_ram.we_n        := false.B
                    io.ext_ram.oe_n        := true.B
                    io.ext_ram.data_out    := dcache_data
                    io.ext_ram.data_out_en := true.B
                }.otherwise {
                    ext_state       := ext_sRead
                    io.ext_ram.we_n := true.B
                    io.ext_ram.oe_n := false.B
                }
                ext_delay_counter := 0.U
            }
        }
        is(ext_sRead) {
            // 保持地址和控制信号稳定
            io.ext_ram.ce_n := false.B
            io.ext_ram.we_n := true.B
            io.ext_ram.oe_n := false.B
            io.ext_ram.addr := ext_addr
            when(ext_delay_counter === (EXECUTION_CYCLE.MEM - 2).U) {
                ext_read_fire := true.B
                ext_state     := ext_sIdle
            }.otherwise {
                ext_delay_counter := ext_delay_counter + 1.U
            }
        }
        is(ext_sWrite) {
            // 保持地址和控制信号稳定
            io.ext_ram.ce_n        := false.B
            io.ext_ram.we_n        := false.B
            io.ext_ram.oe_n        := true.B
            io.ext_ram.data_out_en := true.B
            io.ext_ram.be_n        := ext_be
            io.ext_ram.addr        := ext_addr
            io.ext_ram.data_out    := ext_data
            when(ext_delay_counter === (EXECUTION_CYCLE.MEM - 2).U) {
                ext_state := ext_sIdle
            }.otherwise {
                ext_delay_counter := ext_delay_counter + 1.U
            }
        }
    }

    /* UART 状态机 */
    // UART Data 状态机
    val uart_data_sIdle :: uart_data_sRead :: uart_data_sWrite :: Nil = Enum(3)
    val uart_data_state         = RegInit(uart_data_sIdle)
    val uart_data_delay_counter = RegInit(0.U(2.W))

    // UART Flags 状态机
    val uart_flags_sIdle :: uart_flags_sRead :: Nil = Enum(2)
    val uart_flags_state                            = RegInit(uart_flags_sIdle)
    val uart_flags_delay_counter                    = RegInit(0.U(2.W))

    // UART Data 状态机
    switch(uart_data_state) {
        is(uart_data_sIdle) {
            when(io.dcache_req.valid && dcache_use_uart_data) {
                when(io.dcache_req.we) {
                    uart_data_state  := uart_data_sWrite
                    io.uart.TxDValid := true.B
                    io.uart.TxDData  := io.dcache_req.data(7, 0)
                }.otherwise {
                    uart_data_state := uart_data_sRead
                }
                uart_data_delay_counter := 0.U
            }
        }
        is(uart_data_sRead) {
            when(uart_data_delay_counter === (EXECUTION_CYCLE.MEM - 2).U) {
                uart_data_read_fire := true.B
                uart_data_state     := uart_data_sIdle
            }.otherwise {
                uart_data_delay_counter := uart_data_delay_counter + 1.U
            }
        }
        is(uart_data_sWrite) {
            when(uart_data_delay_counter === (EXECUTION_CYCLE.MEM - 2).U) {
                uart_data_state := uart_data_sIdle
            }.otherwise {
                uart_data_delay_counter := uart_data_delay_counter + 1.U
            }
        }
    }

    // UART Flags 状态机
    switch(uart_flags_state) {
        is(uart_flags_sIdle) {
            when(io.dcache_req.valid && dcache_use_uart_flags && !io.dcache_req.we) {
                uart_flags_state         := uart_flags_sRead
                io.uart.RxdReady         := true.B
                uart_flags_delay_counter := 0.U
            }
        }
        is(uart_flags_sRead) {
            when(uart_flags_delay_counter === (EXECUTION_CYCLE.MEM - 2).U) {
                uart_flags_read_fire := true.B
                uart_flags_state     := uart_flags_sIdle
            }.otherwise {
                uart_flags_delay_counter := uart_flags_delay_counter + 1.U
            }
        }
    }

    // 仲裁输出
    // 优先级：ext > uart_data > uart_flags
    when(ext_read_fire) {
        io.dcache_resp.valid := true.B
        io.dcache_resp.data  := extReadData
    }.elsewhen(uart_data_read_fire) {
        io.dcache_resp.valid := true.B
        io.dcache_resp.uart_data_read := true.B
        io.dcache_resp.data  := uart_read_data_reg2
    }.elsewhen(uart_flags_read_fire) {
        io.dcache_resp.valid := true.B
        io.dcache_resp.data  := uart_read_flags_reg2
    }

    io.dcache_resp.busy := ext_state === ext_sIdle
}
