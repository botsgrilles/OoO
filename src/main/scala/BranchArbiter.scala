// Designed by Kyle. 2025-03-13 18:47
import chisel3._
import circt.stage.ChiselStage
import chisel3.util.PriorityEncoder
import chisel3.util.Cat
import CONSTANTS.RAM_MAP.BASE_RAM_START

/*
    后继PC地址仲裁器

    仲裁 (io.out.PC生成) 逻辑：
    - 如果 pausePC_update 有效，则保留当前 current_PC
    - 否则：
        1. 如果 EX 阶段的 PC_set 有效，则使用传来的 true_PC
        2. 如果 Decode 阶段的 PC_set 有效，则使用传来的 true_PC
        3. 如果分支预测器的dirction_predicted有效，则使用传来的target_predicted
        4. 使用 {PC[31:4] + 1'b0, 4'b0000} 作为默认值
 */

class BranchArbiter extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val pausePC_update      = Input(Bool())
            val EX_PC_set           = Input(Bool())
            val EX_true_PC          = Input(UInt(32.W))
            val Decode_PC_set       = Input(Bool())
            val Decode_true_PC      = Input(UInt(32.W))
            val direction_predicted = Input(Bool())
            val target_predicted    = Input(UInt(32.W))
            val current_PC          = Input(UInt(32.W)) // 当前PC
            val offset_predicted    = Input(UInt(2.W))  // 分支预测器的偏移预测
            val condition_predicted = Input(Bool())     // 是否是条件分支
            val removeMask          = Input(Bool())     // 是否需要移除mask
        }
        val out = new Bundle {
            val PC_arbited       = Output(UInt(32.W))
            val offset_predicted = Output(UInt(2.W))
            val predicted_taken  = Output(Bool()) // 分支预测器的预测结果
            val iCache_refresh   = Output(Bool())
        }
    })

    val reg_PC_arbited       = RegInit(BASE_RAM_START)
    val reg_offset_predicted = RegInit(3.U(2.W))
    val reg_predicted_taken  = RegInit(false.B) // 分支预测器的预测结果
    val iCache_refresh       = RegInit(false.B)

    val misPredictMask = RegNext(io.in.EX_PC_set)

    iCache_refresh := io.in.EX_PC_set || io.in.Decode_PC_set
    val validMask = io.in.current_PC(3, 2).asUInt <= io.in.offset_predicted
    // val masked_direction_predicted = io.in.direction_predicted &&
    //     io.in.current_PC(3, 2).asUInt <= io.in.offset_predicted


    when(io.in.EX_PC_set) {
        reg_PC_arbited       := io.in.EX_true_PC
        reg_offset_predicted := 3.U
        // reg_predicted_taken  := false.B
        misPredictMask := true.B
    }.elsewhen(io.in.Decode_PC_set && !misPredictMask) {
        reg_PC_arbited       := io.in.Decode_true_PC
        reg_offset_predicted := 3.U
        // reg_predicted_taken  := false.B
    }.elsewhen( // 只有分支是条件分支时，才使用方向预测器的结果
      validMask && !io.in.pausePC_update && io.in.current_PC(3, 2).asUInt <= io.in.offset_predicted
    ) {
        reg_PC_arbited       := io.in.target_predicted
        reg_offset_predicted := io.in.offset_predicted
        // reg_predicted_taken  := predict_taken
    }.otherwise {
        reg_PC_arbited := Mux(
          io.in.pausePC_update,
          io.in.current_PC,
          Cat(io.in.current_PC(31, 4) + 1.U(28.W), 0.U(4.W))
        )
    }
    reg_offset_predicted := Mux(io.in.pausePC_update, reg_offset_predicted, io.in.offset_predicted)
    reg_predicted_taken := Mux(io.in.pausePC_update, reg_predicted_taken, io.in.direction_predicted)

    when(reset.asBool){
        reg_PC_arbited       := BASE_RAM_START
        reg_offset_predicted := 3.U
        reg_predicted_taken  := false.B
    }

    io.out.PC_arbited       := reg_PC_arbited
    io.out.offset_predicted := reg_offset_predicted
    io.out.predicted_taken  := reg_predicted_taken
    io.out.iCache_refresh   := iCache_refresh
}

object BranchArbiter extends App {
    ChiselStage.emitSystemVerilogFile(
      new BranchArbiter(),
      Array("--target-dir", "generated")
    )
}
