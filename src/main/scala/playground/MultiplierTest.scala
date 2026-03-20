// Designed by Kyle. 2025-08-02 17:12
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._


class MultiplierTest extends Module {
    val io = IO(new Bundle {
        val A = Input(UInt(32.W))
        val B = Input(UInt(32.W))
        val F = Output(UInt(32.W))
    })
    val reg1 = RegInit(0.U(32.W))
    val reg2 = RegInit(0.U(32.W))
    
    reg1 := io.A * io.B
    reg2 := reg1
    io.F := reg2
}


object MultiplierTest extends App {
    val verilogCode = ChiselStage.emitSystemVerilog(new MultiplierTest())
    println(verilogCode)
    // ChiselStage.emitSystemVerilogFile(new Adder(), Array("--target-dir", "generated"))
}