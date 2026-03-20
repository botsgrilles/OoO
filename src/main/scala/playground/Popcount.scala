package playground

// Designed by Kyle. 2025-08-23 21:39
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class Popcount extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(32.W))
        val out = Output(UInt(6.W))
    })
    
    io.out := PopCount(io.in)
// val tot = 0
//     var oneCount = 0.U(6.W)
//     for (i <- 0 until 32) {
//         oneCount = oneCount + io.in(i)
//     }
    // io.out := oneCount
}


object Popcount extends App {
    val verilogCode = ChiselStage.emitSystemVerilog(new Popcount())
    println(verilogCode)
    // ChiselStage.emitSystemVerilogFile(new Popcount(), Array("--target-dir", "generated"))
}