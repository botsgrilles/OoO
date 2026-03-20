// Designed by Kyle. 2025-03-30 08:42
import chisel3._
import circt.stage.ChiselStage
import chisel3.util.log2Up
import CONSTANTS.CONFIG.nPsyRegs

class Pipline_WR_I(nROBEntries: Int) extends Module {
    class RenameBundle extends Bundle {
        val srcL        = UInt(log2Up(nPsyRegs).W)  // 源操作数1
        val valL        = Bool()     // 源操作数1寄存器是否有效
        val srcR        = UInt(log2Up(nPsyRegs).W)  // 源操作数2
        val valR        = Bool()     // 源操作数2寄存器是否有效
        val dest        = UInt(log2Up(nPsyRegs).W)  // 目的操作数
        val uiL         = UInt(32.W) // 立即数1
        val uiR         = UInt(32.W) // 立即数2（仅供pcaddu12i）
        val branch_inst = UInt(4.W)  // 分支种类
        val FU          = UInt(2.W)  // 使用哪个功能单元
        val ALU_OP      = UInt(4.W)  // ALU操作码
        val reg_write   = Bool()     // 寄存器写使能
        val mem_size    = UInt(2.W)  // 访存大小
        val mem_write   = Bool()     // 内存写使能
        val mem_read    = Bool()     // 内存读使能
        val PC          = UInt(32.W)
        val PC_next     = UInt(32.W)
        val PC_decode   = UInt(32.W)
        val offset_predicted = UInt(2.W) // 分支预测偏移
        val decode_PC_set = Bool()
        val predicted_taken = Bool() // 分支预测结果
    }
    val io = IO(new Bundle {
        val in = new Bundle {
            val pause   = Input(Bool())
            val reset   = Input(Bool())
            val valid   = Input(Vec(4, Bool()))
            val default = Input(Vec(4, new RenameBundle()))
        }
        val out = new Bundle {
            val valid   = Output(Vec(4, Bool()))
            val default = Output(Vec(4, new RenameBundle()))
        }
    })

    val default = RegInit(VecInit(Seq.fill(4)(0.U.asTypeOf(new RenameBundle()))))
    val valid   = RegInit(VecInit(Seq.fill(4)(false.B))) // 寄存器是否有效

    when(io.in.reset || reset.asBool) {
        default := VecInit(Seq.fill(4)(0.U.asTypeOf(new RenameBundle())))
        valid   := VecInit(Seq.fill(4)(false.B)) // Use fill for consistency
    }.elsewhen(io.in.pause) {
        default := default
        // valid   := VecInit(Seq.fill(4)(false.B))
    }.otherwise {
        valid   := io.in.valid
        default := io.in.default
    }
    io.out.valid   := valid
    io.out.default := default
}

object Pipline_WR_I extends App {
    ChiselStage.emitSystemVerilogFile(
      new Pipline_WR_I(nROBEntries = 32),
      Array("--target-dir", "generated")
    )
}
