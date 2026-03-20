// Designed by Kyle. 2025-03-13 14:18
import chisel3._
import circt.stage.ChiselStage
import chisel3.util.Cat

/*
    指令预解码器位于F阶段，在指令被取出后，立刻进行预解码
    筛选出跳转指令，以便快速判断是否需要跳转
    对于 `jirl` 指令，交给 jirl_handler 处理
 */
class Pre_Decoder extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val inst = Input(Vec(4, UInt(32.W))) // 4条指令
        }
        val out = new Bundle {
            val offset       = Output(Vec(4, UInt(26.W))) // 4条指令的偏移量
            val is_jumped    = Output(Vec(4, Bool()))     // 4条指令是否为跳转指令
            val is_jirl      = Output(Vec(4, Bool()))     // 4条指令是否为jirl指令
            val rj           = Output(UInt(5.W))          // 4条指令的寄存器j
            val branch_force = Output(Vec(4, Bool()))     // 是否强制跳转
        }
    })

    io.out.offset       := VecInit(Seq.fill(4)(0.U(26.W)))
    io.out.is_jumped    := VecInit(Seq.fill(4)(false.B))
    io.out.is_jirl      := VecInit(Seq.fill(4)(false.B))
    io.out.branch_force := VecInit(Seq.fill(4)(false.B))
    io.out.rj           := 0.U

    for (i <- 3 until -1 by -1) {
        when(
          io.in.inst(i)(31, 26) === "b010111".U ||  // bne
              io.in.inst(i)(31, 26) === "b010110".U // beq
        ) {
            io.out.is_jumped(i) := true.B
            io.out.offset(i)    := Cat(io.in.inst(i)(25, 10), 0.U(10.W))
        }
            .elsewhen(
              io.in.inst(i)(31, 26) === "b010100".U ||  // b
                  io.in.inst(i)(31, 26) === "b010101".U // bl
            ) {
                io.out.is_jumped(i)    := true.B
                io.out.offset(i)       := io.in.inst(i)(25, 0)
                io.out.branch_force(i) := true.B
            }
        when(io.in.inst(i)(31, 26) === "b010011".U) { // jirl
            io.out.is_jirl(i)      := true.B
            io.out.is_jumped(i)    := true.B
            io.out.offset(i)       := Cat(io.in.inst(i)(25, 10), 0.U(10.W))
            io.out.rj              := io.in.inst(i)(9, 5) // 寄存器j
            io.out.branch_force(i) := true.B
        }
    }
}

object Pre_Decoder extends App {
    ChiselStage.emitSystemVerilogFile(
      new Pre_Decoder(),
      Array("--target-dir", "generated")
    )
}
