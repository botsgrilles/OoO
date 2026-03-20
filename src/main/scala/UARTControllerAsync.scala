// Designed by Kyle. 2025-07-05 14:34
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import asyncqueue.modules._
import CONSTANTS.CONFIG._

// Async Receiver BlackBox
class async_receiver2(
  clkFrequency: Int = 11_059_200,
  baud: Int = BaudRate,
  Oversampling: Int = 8
) extends BlackBox(
      Map("ClkFrequency" -> clkFrequency, "Baud" -> baud, "Oversampling" -> Oversampling)
    ) {
    val io = IO(new Bundle {
        val clk            = Input(Clock())
        val RxD            = Input(Bool())
        val RxD_data_ready = Output(Bool())
        val RxD_clear      = Input(Bool())
        val RxD_data       = Output(UInt(8.W))
    })
    override def desiredName: String = "async_receiver"
}

// Async Transmitter BlackBox
class async_transmitter2(clkFrequency: Int = 11_059_200, baud: Int = BaudRate)
    extends BlackBox(Map("ClkFrequency" -> clkFrequency, "Baud" -> baud)) {
    val io = IO(new Bundle {
        val clk       = Input(Clock())
        val TxD       = Output(Bool())
        val TxD_busy  = Output(Bool())
        val TxD_start = Input(Bool())
        val TxD_data  = Input(UInt(8.W))
    })
    override def desiredName: String = "async_transmitter"
}

// 支持推测执行的同步循环FIFO
class SpeculativeFIFO(depth: Int, dataWidth: Int = 8) extends Module {
    val io = IO(new Bundle {
        // 写入接口
        val enq = Flipped(new DecoupledIO(UInt(dataWidth.W)))

        // 读取接口（推测读取）
        val deq = new DecoupledIO(UInt(dataWidth.W))

        // 控制信号
        val commit  = Input(Bool())
        val recover = Input(Bool())

        // 状态输出
        val empty = Output(Bool())
        val full  = Output(Bool())
        val count = Output(UInt(log2Ceil(depth + 1).W))
    })

    require(isPow2(depth), "FIFO depth must be power of 2")

    val ptrWidth = log2Ceil(depth)

    // 内存和指针
    val mem   = Mem(depth, UInt(dataWidth.W))
    val tail  = RegInit(0.U(ptrWidth.W)) // 尾指针（写入位置）
    val chead = RegInit(0.U(ptrWidth.W)) // 已提交头指针
    val shead = RegInit(0.U(ptrWidth.W)) // 推测头指针（读取位置）

    // 计算各种状态
    val committed_count   = Mux(tail >= chead, tail - chead, (depth.U - chead) + tail)
    val speculative_count = Mux(shead >= chead, shead - chead, (depth.U - chead) + shead)
    val total_count       = Mux(tail >= chead, tail - chead, (depth.U - chead) + tail)

    val is_empty      = (tail === chead)
    val is_full       = (total_count === (depth - 1).U)
    val can_spec_read = (shead =/= tail) // 推测读取不能超过尾指针

    // 写入逻辑
    val do_enq = io.enq.valid && io.enq.ready
    io.enq.ready := !is_full

    when(do_enq) {
        mem(tail) := io.enq.bits
        tail      := Mux(tail === (depth - 1).U, 0.U, tail + 1.U)
    }

    // 推测读取逻辑
    val do_deq = io.deq.valid && io.deq.ready
    io.deq.valid := can_spec_read
    io.deq.bits  := mem(shead)

    when(do_deq) {
        shead := Mux(shead === (depth - 1).U, 0.U, shead + 1.U)
    }

    when(io.commit) {
        // chead := shead
        chead := Mux(chead === (depth - 1).U, 0.U, chead + 1.U)
    }

    // Recover逻辑：重置shead到chead
    when(io.recover) {
        shead := chead
    }

    // 输出状态
    io.empty := is_empty
    io.full  := is_full
    io.count := total_count
}

/*
    UART Controller
    使用AsyncFIFO缓冲的UART控制器，支持异步时钟域
 */
class UARTControllerAsync(FIFODepth: Int) extends Module {
    val io = IO(new Bundle {
        val uart = new Bundle {
            val rxd = Input(Bool())
            val txd = Output(Bool())
        }
        val cpu = new Bundle {
            val TxDReady = Output(Bool())
            val TxDValid = Input(Bool())
            val TxDData  = Input(UInt(8.W))

            val RxdReady = Input(Bool())
            val RxdValid = Output(Bool())
            val RxdData  = Output(UInt(8.W))
        }
        val clock_serial = Input(Clock()) // 串口时钟输入
        val commit       = Input(Bool())
        val recover      = Input(Bool())
        // val reset_serial = Input(Bool())  // 串口复位输入
    })

    // AsyncFIFO参数配置
    val fifoParams =
        AsyncQueueParams(depth = FIFODepth, sync = 3, safe = true, narrow = false)

    // 实例化异步FIFO（第一级缓冲）
    val rxd_async_fifo = Module(new AsyncQueue(UInt(8.W), fifoParams))
    val txd_fifo       = Module(new AsyncQueue(UInt(8.W), fifoParams))

    // 实例化推测FIFO（第二级缓冲，仅用于接收）
    val rxd_spec_fifo = Module(new SpeculativeFIFO(FIFODepth))

    // 实例化UART模块
    val uart_receiver    = Module(new async_receiver2())
    val uart_transmitter = Module(new async_transmitter2())

    // RXD异步FIFO: 串口时钟域(enq) -> CPU时钟域(deq)
    rxd_async_fifo.io.enq_clock := io.clock_serial
    rxd_async_fifo.io.enq_reset := false.B
    rxd_async_fifo.io.deq_clock := clock
    rxd_async_fifo.io.deq_reset := reset.asBool

    // TXD FIFO: CPU时钟域(enq) -> 串口时钟域(deq)
    txd_fifo.io.enq_clock := clock
    txd_fifo.io.enq_reset := reset.asBool
    txd_fifo.io.deq_clock := io.clock_serial
    txd_fifo.io.deq_reset := false.B

    // 接收器连接 (串口时钟域)
    uart_receiver.io.clk := io.clock_serial
    uart_receiver.io.RxD := io.uart.rxd

    // 接收数据写入异步FIFO (串口时钟域)
    rxd_async_fifo.io.enq.valid := uart_receiver.io.RxD_data_ready
    rxd_async_fifo.io.enq.bits  := uart_receiver.io.RxD_data
    uart_receiver.io.RxD_clear  := uart_receiver.io.RxD_data_ready || reset.asBool

    // 异步FIFO到推测FIFO的数据传输（CPU时钟域）
    rxd_spec_fifo.io.enq <> rxd_async_fifo.io.deq

    // 推测FIFO的控制信号
    rxd_spec_fifo.io.commit  := io.commit
    rxd_spec_fifo.io.recover := io.recover

    // 发送器连接 (串口时钟域)
    uart_transmitter.io.clk := io.clock_serial
    io.uart.txd             := uart_transmitter.io.TxD

    /*
    // 从FIFO读取数据发送 (串口时钟域) - 修复版本
    withClock(io.clock_serial) {
        val tx_start_reg = RegInit(false.B)
        val tx_data_reg  = RegInit(0.U(8.W))
        // 状态机：确保数据发送的可靠性
        when(!uart_transmitter.io.TxD_busy && txd_fifo.io.deq.valid && !tx_start_reg) {
            // 发送器空闲且FIFO有数据，开始发送
            tx_start_reg := true.B
            tx_data_reg  := txd_fifo.io.deq.bits
        }.elsewhen(tx_start_reg && uart_transmitter.io.TxD_busy) {
            // 发送器已经开始工作，清除start信号
            tx_start_reg := false.B
        }

        // 连接到发送器
        uart_transmitter.io.TxD_start := tx_start_reg
        uart_transmitter.io.TxD_data  := tx_data_reg

        // 只有在确实开始发送时才从FIFO读取
        txd_fifo.io.deq.ready := tx_start_reg && uart_transmitter.io.TxD_busy
    }
     */

    txd_fifo.io.deq.ready := !uart_transmitter.io.TxD_busy
    uart_transmitter.io.TxD_start := txd_fifo.io.deq.valid && !uart_transmitter.io.TxD_busy
    uart_transmitter.io.TxD_data := txd_fifo.io.deq.bits

    // CPU接口连接 (CPU时钟域)
    // 发送接口
    txd_fifo.io.enq.valid := io.cpu.TxDValid
    txd_fifo.io.enq.bits  := io.cpu.TxDData
    io.cpu.TxDReady       := txd_fifo.io.enq.ready

    // 接收接口（通过推测FIFO）
    rxd_spec_fifo.io.deq.ready := io.cpu.RxdReady
    io.cpu.RxdValid            := rxd_spec_fifo.io.deq.valid
    io.cpu.RxdData             := rxd_spec_fifo.io.deq.bits

    if (advancedDebug) {
        val debug_module = Module(new Module {
            val io = IO(new Bundle {
                val input_valid    = Input(Bool())
                val trigger_char   = Input(UInt(8.W))
                val output_char    = Input(UInt(8.W))
                val output_valid   = Input(Bool())
                val error_position = Output(UInt(8.W))
            })

            // 预期的输出序列
            val expected_sequence = VecInit(
              Seq(
                0x0c.U,
                0x04.U,
                0x80.U,
                0x02.U,
                0x0d.U,
                0x04.U,
                0x80.U,
                0x02.U,
                0x04.U,
                0x80.U,
                0x00.U,
                0x15.U,
                0x85.U,
                0x80.U,
                0x80.U,
                0x02.U,
                0x8e.U,
                0x35.U,
                0x10.U,
                0x00.U,
                0xac.U,
                0x01.U,
                0x80.U,
                0x02.U,
                0xcd.U,
                0x01.U,
                0x80.U,
                0x02.U,
                0x8e.U,
                0x00.U,
                0x80.U,
                0x29.U,
                0x84.U,
                0x10.U,
                0x80.U,
                0x02.U,
                0x85.U,
                0xec.U,
                0xff.U,
                0x5f.U,
                0x20.U,
                0x00.U,
                0x00.U,
                0x4c.U
              )
            )

            val sequence_length = expected_sequence.length.U

            // 状态寄存器
            val monitoring     = RegInit(false.B)
            val error_detected = RegInit(false.B)
            val position       = RegInit(0.U(8.W))
            val error_pos      = RegInit(0.U(8.W))

            debugSignal(monitoring)
            debugSignal(error_detected)

            // 检测触发字符 'D' (0x44)
            when(io.trigger_char === 0x44.U && !monitoring && io.input_valid) {
                monitoring := true.B
                position   := 0.U
                error_pos  := 0.U
            }

            // 监听输出序列
            when(monitoring && io.output_valid) {
                when(position < sequence_length) {
                    when(io.output_char === expected_sequence(position)) {
                        // 正确，继续下一个
                        position := position + 1.U
                    }.otherwise {
                        // 错误，记录位置并停止监听
                        error_pos      := position
                        error_detected := true.B
                        monitoring     := false.B
                    }
                }.otherwise {
                    // 序列完成
                    monitoring := false.B
                }
            }

            io.error_position := error_pos
        })

        // 连接调试模块
        debug_module.io.trigger_char := rxd_spec_fifo.io.deq.bits
        debug_module.io.input_valid := io.cpu.RxdValid && io.cpu.RxdReady 
        debug_module.io.output_char  := io.cpu.TxDData
        debug_module.io.output_valid := io.cpu.TxDValid && io.cpu.TxDReady
    
    debugSignal(debug_module.io)
    }
}

object UARTController extends App {
    ChiselStage.emitSystemVerilogFile(
      new UARTControllerAsync(FIFODepth = 16),
      Array("--target-dir", "generated")
    )
}
