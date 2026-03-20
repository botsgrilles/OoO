package playground

// Designed by Kyle. 2025-08-19 08:25
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._


class TestCat extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(4.W))
        val out = Output(UInt(6.W))
        val mux_out = Output(UInt(3.W))
    })
    io.out := Cat(io.in, "b00".U)

    io.mux_out := Mux1H(io.in, Seq(
        2.U,
        3.U,
        4.U,
        5.U
    ))
}


object TestCat extends App {
    val verilogCode = ChiselStage.emitSystemVerilog(new TestCat())
    println(verilogCode)
    // ChiselStage.emitSystemVerilogFile(new Cat(), Array("--target-dir", "generated"))
}