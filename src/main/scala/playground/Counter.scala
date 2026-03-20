// Designed by Kyle. 2025-02-13 20:27
import chisel3._
import circt.stage.ChiselStage
import chisel3.util.log2Ceil

class Counter(max: Int) extends Module {
    val io = IO(new Bundle {
        val enable = Input(Bool())
        val out    = Output(UInt(log2Ceil(max + 1).W))
    })

    val count = RegInit(0.U(log2Ceil(max + 1).W))

    when(io.enable) {
        count := Mux(count === max.U, 0.U, count + 1.U)
    }

    when(reset.asBool) {
        count := 0.U
    }

    io.out := count
}

object Counter extends App {
    ChiselStage.emitSystemVerilogFile(
      new Counter(100),
      Array("--target-dir", "generated")
    )
}
