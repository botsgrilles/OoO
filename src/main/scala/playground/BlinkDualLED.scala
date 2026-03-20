// Designed by Kyle. 2025-03-16 09:14
import chisel3._
import circt.stage.ChiselStage


class BlinkDualLED extends Module {
    val io = IO(new Bundle {
        val out = new Bundle {
            val led0_red = Bool()
            val led0_green = Bool()
            val led1_red = Bool()
            val led1_green = Bool()
        }
    })

    // Repeat: RR RG GG GR
    val led0_red = RegInit(false.B)
    val led0_green = RegInit(false.B)
    val led1_red = RegInit(false.B)
    val led1_green = RegInit(false.B)

    val clock_divder = RegInit(0.U(32.W))

    // 100MHz Clock, devide to 2Hz
    val counter = RegInit(0.U(2.W))
    when (clock_divder === (50000000.U - 1.U)) {
        clock_divder := 0.U
        counter := counter + 1.U
    } .otherwise {
        clock_divder := clock_divder + 1.U
    }


    when (counter === 0.U) {
        led0_red := true.B
        led0_green := false.B
        led1_red := true.B
        led1_green := false.B
    } .elsewhen (counter === 1.U) {
        led0_red := true.B
        led0_green := false.B
        led1_red := false.B
        led1_green := true.B
    } .elsewhen (counter === 2.U) {
        led0_red := false.B
        led0_green := true.B
        led1_red := false.B
        led1_green := true.B
    } .otherwise {
        led0_red := false.B
        led0_green := true.B
        led1_red := true.B
        led1_green := false.B
    }

    io.out.led0_red := led0_red
    io.out.led0_green := led0_green
    io.out.led1_red := led1_red
    io.out.led1_green := led1_green
}


object BlinkDualLED extends App {
    // val verilogCode = ChiselStage.emitSystemVerilog(new BlinkDualLED())
    // println(verilogCode)
    ChiselStage.emitSystemVerilogFile(new BlinkDualLED(), Array("--target-dir", "generated"))
}