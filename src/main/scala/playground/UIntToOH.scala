// Designed by Kyle. 2025-08-17 18:58
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._


class UIntToOHTest extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(6.W))
        val out1 = Output(UInt(64.W))
        val out2 = Output(UInt(64.W))
    })

    io.out1 := UIntToOH(io.in, 64)
    
    val out2 = VecInit(Seq.fill(64)(0.U(1.W)))
    out2(io.in) := 1.U
    io.out2 := out2.asUInt
}

object UIntToOHTest extends App {
    ChiselStage.emitSystemVerilogFile(new UIntToOHTest(), Array("--target-dir", "generated"))
}