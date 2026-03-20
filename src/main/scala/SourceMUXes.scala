// Designed by Kyle. 2025-03-14 20:38
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import CONSTANTS.CONFIG._
import CONSTANTS.MEM_SIZE
/*
    运算组件之前的多路选择器以及地址加法器
    为数据前馈提供通道
 */

class MUX_ALU extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val uiL        = Input(UInt(32.W)) // 立即数
            val RD         = Input(UInt(32.W)) // 寄存器读出
            val broadcast0 = Input(UInt(32.W))
            val broadcast1 = Input(UInt(32.W))
            val broadcast2 = Input(UInt(32.W))
            val broadcast3 = Input(UInt(32.W))

            val choice = Input(UInt(6.W))
            val stall  = Input(Bool())
        }
        val out = new Bundle {
            val result = Output(UInt(32.W))
        }
    })

    val freeze = RegInit(false.B)
    // val currentVal = MuxLookup(io.in.choice, 0.U)(
    //   Array(
    //     0.U -> io.in.uiL,
    //     1.U -> io.in.RD,
    //     2.U -> io.in.broadcast0,
    //     3.U -> io.in.broadcast1,
    //     4.U -> io.in.broadcast2,
    //     5.U -> io.in.broadcast3
    //   )
    // )
    val currentVal = Mux1H(io.in.choice, Seq(
        io.in.uiL,
        io.in.RD,
        io.in.broadcast0,
        io.in.broadcast1,
        io.in.broadcast2,
        io.in.broadcast3
    ))


    val regVal = RegInit(0.U(32.W))

    when(!freeze) {
        io.out.result := currentVal
        when(io.in.stall) {
            freeze := true.B
            regVal := currentVal
        }
    }.otherwise {
        io.out.result := regVal
        when(!io.in.stall) {
            freeze := false.B
        }
    }
}

class MUX_MEM extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val uiL        = Input(UInt(32.W)) // 立即数
            val RDL        = Input(UInt(32.W))
            val RDR        = Input(UInt(32.W))
            val broadcast0 = Input(UInt(32.W))
            val broadcast1 = Input(UInt(32.W))
            val broadcast2 = Input(UInt(32.W))
            val broadcast3 = Input(UInt(32.W))

            val choiceL = Input(UInt(6.W))
            val choiceR = Input(UInt(6.W))

            val stallL = Input(Bool())
            val stallR = Input(Bool())
        }
        val out = new Bundle {
            val WAddr = Output(UInt(32.W))
            val WData = Output(UInt(32.W))
        }
    })
    // val currentValL = MuxLookup(io.in.choiceL, 0.U)(
    //   Array(
    //     0.U -> io.in.uiL,
    //     1.U -> io.in.RDL,
    //     2.U -> io.in.broadcast0,
    //     3.U -> io.in.broadcast1,
    //     4.U -> io.in.broadcast2,
    //     5.U -> io.in.broadcast3
    //   )
    // )
    val currentValL = Mux1H(io.in.choiceL, Seq(
        io.in.uiL,
        io.in.RDL,
        io.in.broadcast0,
        io.in.broadcast1,
        io.in.broadcast2,
        io.in.broadcast3
    ))
    // val currentValR = MuxLookup(io.in.choiceR, 0.U)(
    //   Array(
    //     0.U -> io.in.uiL,
    //     1.U -> io.in.RDR,
    //     2.U -> io.in.broadcast0,
    //     3.U -> io.in.broadcast1,
    //     4.U -> io.in.broadcast2,
    //     5.U -> io.in.broadcast3
    //   )
    // )
    val currentValR = Mux1H(io.in.choiceR, Seq(
        io.in.uiL,
        io.in.RDR,
        io.in.broadcast0,
        io.in.broadcast1,
        io.in.broadcast2,
        io.in.broadcast3
    ))

    debugSignal(currentValL)
    debugSignal(currentValR)

    val regValL = RegInit(0.U(32.W))
    val regValR = RegInit(0.U(32.W))

    val freezeL = RegInit(false.B)
    val freezeR = RegInit(false.B)

    // 分离stallL和stallR
    val n = log2Ceil(DataRAMSize)
    when(!freezeL) {
        io.out.WAddr := Cat(currentValL(31, n), (io.in.uiL(n - 1, 0).asSInt + currentValL(n - 1, 0).asSInt).asUInt)
        when(io.in.stallL) {
            freezeL := true.B
            regValL := currentValL
        }
    }.otherwise {
        io.out.WAddr := Cat(regValL(31, n), (io.in.uiL(n - 1, 0).asSInt + regValL(n - 1, 0).asSInt).asUInt)
        when(!io.in.stallL) {
            freezeL := false.B
        }
    }

    when(!freezeR) {
        io.out.WData := currentValR
        when(io.in.stallR) {
            freezeR := true.B
            regValR := currentValR
        }
    }.otherwise {
        io.out.WData := regValR
        when(!io.in.stallR) {
            freezeR := false.B
        }
    }
}

class MUX_MUL extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val RD         = Input(UInt(32.W))
            val broadcast0 = Input(UInt(32.W))
            val broadcast1 = Input(UInt(32.W))
            val broadcast2 = Input(UInt(32.W))
            val broadcast3 = Input(UInt(32.W))

            val choice = Input(UInt(6.W))
            val stall  = Input(Bool())
        }
        val out = new Bundle {
            val result = Output(UInt(32.W))
        }
    })
    // val currentVal = MuxLookup(io.in.choice, 0.U)(
    //   Array(
    //     1.U -> io.in.RD,
    //     2.U -> io.in.broadcast0,
    //     3.U -> io.in.broadcast1,
    //     4.U -> io.in.broadcast2,
    //     5.U -> io.in.broadcast3
    //   )
    // )
    val currentVal = Mux1H(io.in.choice, Seq(
        0.U,
        io.in.RD,
        io.in.broadcast0,
        io.in.broadcast1,
        io.in.broadcast2,
        io.in.broadcast3
    ))

    val regVal = RegInit(0.U(32.W))
    val freeze = RegInit(false.B)

    when(!freeze) {
        io.out.result := currentVal
        when(io.in.stall) {
            freeze := true.B
            regVal := currentVal
        }
    }.otherwise {
        io.out.result := regVal
        when(!io.in.stall) {
            freeze := false.B
        }
    }
}

class SourceMUXes extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val ALU0 = new Bundle {
                val uiL     = Input(UInt(32.W)) // 立即数
                val uiR     = Input(UInt(32.W)) // 立即数
                val RDL     = Input(UInt(32.W)) // 寄存器读出
                val RDR     = Input(UInt(32.W)) // 寄存器读出
                val choiceL = Input(UInt(6.W))
                val choiceR = Input(UInt(6.W))
                val stallL  = Input(Bool())
                val stallR  = Input(Bool())
            }
            val ALU1 = new Bundle {
                val uiL     = Input(UInt(32.W)) // 立即数
                val uiR     = Input(UInt(32.W)) // 立即数
                val RDL     = Input(UInt(32.W)) // 寄存器读出
                val RDR     = Input(UInt(32.W)) // 寄存器读出
                val choiceL = Input(UInt(6.W))
                val choiceR = Input(UInt(6.W))
                val stallL  = Input(Bool())
                val stallR  = Input(Bool())
            }
            val broadcast = Vec(4, Input(UInt(32.W))) // 广播寄存器
            val MEM = new Bundle {
                val ui      = Input(UInt(32.W)) // 立即数
                val RDL     = Input(UInt(32.W)) // 寄存器读出
                val RDR     = Input(UInt(32.W)) // 寄存器读出
                val choiceL = Input(UInt(6.W))
                val choiceR = Input(UInt(6.W))
                val stallL  = Input(Bool())
                val stallR  = Input(Bool())
            }
            val MUL = new Bundle {
                val RDL     = Input(UInt(32.W)) // 寄存器读出
                val RDR     = Input(UInt(32.W)) // 寄存器读出
                val choiceL = Input(UInt(6.W))
                val choiceR = Input(UInt(6.W))
                val stallL  = Input(Bool())
                val stallR  = Input(Bool())
            }
        }
        val out = new Bundle {
            val ALU0 = new Bundle {
                val L = Output(UInt(32.W))
                val R = Output(UInt(32.W))
            }
            val ALU1 = new Bundle {
                val L = Output(UInt(32.W))
                val R = Output(UInt(32.W))
            }
            val MEM = new Bundle {
                val WAddr = Output(UInt(32.W))
                val WData = Output(UInt(32.W))
            }
            val MUL = new Bundle {
                val L = Output(UInt(32.W))
                val R = Output(UInt(32.W))
            }
        }
    })

    val MUX_ALU0_L = Module(new MUX_ALU())
    val MUX_ALU0_R = Module(new MUX_ALU())
    val MUX_ALU1_L = Module(new MUX_ALU())
    val MUX_ALU1_R = Module(new MUX_ALU())
    val MUX_MEM    = Module(new MUX_MEM())
    val MUX_MUL_L  = Module(new MUX_MUL())
    val MUX_MUL_R  = Module(new MUX_MUL())

    MUX_ALU0_L.io.in.uiL        := io.in.ALU0.uiL
    MUX_ALU0_L.io.in.RD         := io.in.ALU0.RDL
    MUX_ALU0_L.io.in.broadcast0 := io.in.broadcast(0)
    MUX_ALU0_L.io.in.broadcast1 := io.in.broadcast(1)
    MUX_ALU0_L.io.in.broadcast2 := io.in.broadcast(2)
    MUX_ALU0_L.io.in.broadcast3 := io.in.broadcast(3)
    MUX_ALU0_L.io.in.choice     := io.in.ALU0.choiceL
    MUX_ALU0_L.io.in.stall      := io.in.ALU0.stallL
    io.out.ALU0.L               := MUX_ALU0_L.io.out.result

    MUX_ALU0_R.io.in.uiL        := io.in.ALU0.uiR
    MUX_ALU0_R.io.in.RD         := io.in.ALU0.RDR
    MUX_ALU0_R.io.in.broadcast0 := io.in.broadcast(0)
    MUX_ALU0_R.io.in.broadcast1 := io.in.broadcast(1)
    MUX_ALU0_R.io.in.broadcast2 := io.in.broadcast(2)
    MUX_ALU0_R.io.in.broadcast3 := io.in.broadcast(3)
    MUX_ALU0_R.io.in.choice     := io.in.ALU0.choiceR
    MUX_ALU0_R.io.in.stall      := io.in.ALU0.stallR
    io.out.ALU0.R               := MUX_ALU0_R.io.out.result

    MUX_ALU1_L.io.in.uiL        := io.in.ALU1.uiL
    MUX_ALU1_L.io.in.RD         := io.in.ALU1.RDL
    MUX_ALU1_L.io.in.broadcast0 := io.in.broadcast(0)
    MUX_ALU1_L.io.in.broadcast1 := io.in.broadcast(1)
    MUX_ALU1_L.io.in.broadcast2 := io.in.broadcast(2)
    MUX_ALU1_L.io.in.broadcast3 := io.in.broadcast(3)
    MUX_ALU1_L.io.in.choice     := io.in.ALU1.choiceL
    MUX_ALU1_L.io.in.stall      := io.in.ALU1.stallL
    io.out.ALU1.L               := MUX_ALU1_L.io.out.result

    MUX_ALU1_R.io.in.uiL        := io.in.ALU1.uiR
    MUX_ALU1_R.io.in.RD         := io.in.ALU1.RDR
    MUX_ALU1_R.io.in.broadcast0 := io.in.broadcast(0)
    MUX_ALU1_R.io.in.broadcast1 := io.in.broadcast(1)
    MUX_ALU1_R.io.in.broadcast2 := io.in.broadcast(2)
    MUX_ALU1_R.io.in.broadcast3 := io.in.broadcast(3)
    MUX_ALU1_R.io.in.choice     := io.in.ALU1.choiceR
    MUX_ALU1_R.io.in.stall      := io.in.ALU1.stallR
    io.out.ALU1.R               := MUX_ALU1_R.io.out.result

    MUX_MEM.io.in.uiL        := io.in.MEM.ui
    MUX_MEM.io.in.RDL        := io.in.MEM.RDL
    MUX_MEM.io.in.RDR        := io.in.MEM.RDR
    MUX_MEM.io.in.broadcast0 := io.in.broadcast(0)
    MUX_MEM.io.in.broadcast1 := io.in.broadcast(1)
    MUX_MEM.io.in.broadcast2 := io.in.broadcast(2)
    MUX_MEM.io.in.broadcast3 := io.in.broadcast(3)
    MUX_MEM.io.in.choiceL    := io.in.MEM.choiceL
    MUX_MEM.io.in.choiceR    := io.in.MEM.choiceR
    MUX_MEM.io.in.stallL     := io.in.MEM.stallL
    MUX_MEM.io.in.stallR     := io.in.MEM.stallR
    io.out.MEM.WAddr         := MUX_MEM.io.out.WAddr
    io.out.MEM.WData         := MUX_MEM.io.out.WData

    MUX_MUL_L.io.in.RD         := io.in.MUL.RDL
    MUX_MUL_L.io.in.broadcast0 := io.in.broadcast(0)
    MUX_MUL_L.io.in.broadcast1 := io.in.broadcast(1)
    MUX_MUL_L.io.in.broadcast2 := io.in.broadcast(2)
    MUX_MUL_L.io.in.broadcast3 := io.in.broadcast(3)
    MUX_MUL_L.io.in.stall     := io.in.MUL.stallL
    MUX_MUL_L.io.in.choice     := io.in.MUL.choiceL

    MUX_MUL_R.io.in.RD         := io.in.MUL.RDR
    MUX_MUL_R.io.in.broadcast0 := io.in.broadcast(0)
    MUX_MUL_R.io.in.broadcast1 := io.in.broadcast(1)
    MUX_MUL_R.io.in.broadcast2 := io.in.broadcast(2)
    MUX_MUL_R.io.in.broadcast3 := io.in.broadcast(3)
    MUX_MUL_R.io.in.choice     := io.in.MUL.choiceR
    MUX_MUL_R.io.in.stall      := io.in.MUL.stallR
    io.out.MUL.L               := MUX_MUL_L.io.out.result
    io.out.MUL.R               := MUX_MUL_R.io.out.result
}

object SourceMUXes extends App {
    ChiselStage.emitSystemVerilogFile(
      new SourceMUXes(),
      Array("--target-dir", "generated")
    )
}
