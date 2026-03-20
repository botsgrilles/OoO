// Designed by Kyle. 2025-07-05 14:34
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import  CONSTANTS.CONFIG._

// Async Receiver BlackBox
class async_receiver(clkFrequency: Int = 11_059_200, baud: Int = BaudRate) extends BlackBox(Map(
    "ClkFrequency" -> clkFrequency,
    "Baud" -> baud
)) {
    val io = IO(new Bundle {
        val clk = Input(Clock())
        val RxD = Input(Bool())
        val RxD_data_ready = Output(Bool())
        val RxD_clear = Input(Bool())
        val RxD_data = Output(UInt(8.W))
    })
}

// Async Transmitter BlackBox
class async_transmitter(clkFrequency: Int = 11_059_200, baud: Int = BaudRate) extends BlackBox(Map(
    "ClkFrequency" -> clkFrequency,
    "Baud" -> baud
)) {
    val io = IO(new Bundle {
        val clk = Input(Clock())
        val TxD = Output(Bool())
        val TxD_busy = Output(Bool())
        val TxD_start = Input(Bool())
        val TxD_data = Input(UInt(8.W))
    })
}

// Simple FIFO Module
class SimpleFIFO(depth: Int = 16) extends Module {
    val io = IO(new Bundle {
        val wr_en = Input(Bool())
        val din = Input(UInt(8.W))
        val full = Output(Bool())
        val rd_en = Input(Bool())
        val dout = Output(UInt(8.W))
        val empty = Output(Bool())
    })

    val fifo = Module(new Queue(UInt(8.W), depth))

    fifo.io.enq.valid := io.wr_en
    fifo.io.enq.bits := io.din
    io.full := !fifo.io.enq.ready

    fifo.io.deq.ready := io.rd_en
    io.dout := fifo.io.deq.bits
    io.empty := !fifo.io.deq.valid
}

/* 
    UART Controller
    使用FIFO缓冲的UART控制器
 */
class UARTControllerSync(FIFODepth: Int) extends Module {
    val io = IO(new Bundle {
        val uart = new Bundle{
            val rxd = Input(Bool())
            val txd = Output(Bool())
        }
        val cpu = new Bundle {
            val TxDReady = Output(Bool())
            val TxDValid = Input(Bool())
            val TxDData = Input(UInt(8.W))

            val RxdReady = Input(Bool())
            val RxdValid = Output(Bool())
            val RxdData = Output(UInt(8.W))
        }
        val clock_serial = Input(Clock())
    })

    // 实例化模块
    val uart_receiver = Module(new async_receiver())
    val uart_transmitter = Module(new async_transmitter())
    val rxd_fifo = Module(new SimpleFIFO())
    val txd_fifo = Module(new SimpleFIFO())

    // 接收器连接
    uart_receiver.io.clk := io.clock_serial
    uart_receiver.io.RxD := io.uart.rxd
    uart_receiver.io.RxD_clear := (uart_receiver.io.RxD_data_ready && !rxd_fifo.io.full) || reset.asBool

    // 发送器连接
    uart_transmitter.io.clk := io.clock_serial
    io.uart.txd := uart_transmitter.io.TxD
    uart_transmitter.io.TxD_start := !uart_transmitter.io.TxD_busy && !txd_fifo.io.empty
    uart_transmitter.io.TxD_data := txd_fifo.io.dout

    // RXD FIFO连接
    rxd_fifo.io.wr_en := uart_receiver.io.RxD_clear
    rxd_fifo.io.din := uart_receiver.io.RxD_data
    rxd_fifo.io.rd_en := RegNext(io.cpu.RxdReady)

    // TXD FIFO连接
    txd_fifo.io.wr_en := io.cpu.TxDValid
    txd_fifo.io.din := io.cpu.TxDData
    txd_fifo.io.rd_en := uart_transmitter.io.TxD_start

    // CPU接口连接
    io.cpu.TxDReady := !txd_fifo.io.full
    io.cpu.RxdValid := !rxd_fifo.io.empty
    io.cpu.RxdData := rxd_fifo.io.dout
}

object UARTControllerSync extends App {
    ChiselStage.emitSystemVerilogFile(new UARTControllerSync(FIFODepth = 16), Array("--target-dir", "generated"))
}