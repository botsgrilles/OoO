// Designed by Kyle. 2025-03-12 19:55
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import CONSTANTS.CONFIG.debugSignal

class Pipline_F_D extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val valid            = Input(Bool())    // 是否有效
            val PC_next          = Input(UInt(32.W))
            val offset_predicted = Input(UInt(2.W)) // 分支预测器的偏移预测
            val predicted_taken  = Input(Bool())    // 分支预测器的预测结果
            val instruction      = Input(Vec(4, UInt(32.W)))
            val refetch_D        = Input(Bool())    // 是否需要重新获取指令
            val pause            = Input(Bool())
            val clear            = Input(Bool())
        }
        val out = new Bundle {
            val PC               = Output(UInt(32.W))
            val offset_predicted = Output(UInt(2.W))
            val PC_next          = Output(UInt(32.W))
            val predicted_taken  = Output(Bool())
            val instruction      = Output(Vec(4, UInt(32.W)))
            val valid            = Output(Vec(4, Bool()))
        }
    })

    val PC_delayed       = RegNext(io.in.PC_next, 0.U((32.W)))
    val PC               = RegInit(0.U(32.W))
    val PC_next          = RegInit(0.U(32.W))
    val offset_predicted = RegInit(0.U(2.W))
    val predicted_taken  = RegInit(false.B)
    val instruction      = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
    val valid            = RegInit(VecInit(Seq(false.B, false.B, false.B, false.B)))
    val PC_offset        = PC_next(3, 2)

    val instruction_reserved = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))

    val sNormal :: sPause :: sClear :: Nil = Enum(3)
    val state                              = RegInit(sNormal)

    // 根据PC_offset生成valid_normal向量
    val valid_normal = WireInit(VecInit(Seq.fill(4)(false.B)))
    switch(PC_offset) {
        is(0.U) { valid_normal := VecInit(Seq(true.B, true.B, true.B, true.B)) }
        is(1.U) { valid_normal := VecInit(Seq(false.B, true.B, true.B, true.B)) }
        is(2.U) { valid_normal := VecInit(Seq(false.B, false.B, true.B, true.B)) }
        is(3.U) { valid_normal := VecInit(Seq(false.B, false.B, false.B, true.B)) }
    }

    debugSignal(valid_normal)

    val reFetch_mask =
        (io.in.refetch_D || RegNext(io.in.refetch_D, false.B))
    debugSignal(reFetch_mask)

    // when(io.in.misPredict) {
    //     misPredictMask := true.B
    // }

    // when(io.in.removeMask) {
    //     misPredictMask := false.B
    // }

    switch(state) {
        is(sNormal) {
            when(io.in.clear || reset.asBool || reFetch_mask) {
                PC                   := PC_next
                PC_next              := io.in.PC_next
                offset_predicted     := io.in.offset_predicted
                predicted_taken      := io.in.predicted_taken
                instruction          := VecInit(Seq.fill(4)(0.U(32.W)))
                instruction_reserved := VecInit(Seq.fill(4)(0.U(32.W)))
                valid                := VecInit(Seq.fill(4)(false.B))
                when(io.in.clear || reset.asBool) {
                    state := sClear
                }
            }.elsewhen(io.in.pause) {
                PC_next          := PC_next
                PC               := PC
                offset_predicted := offset_predicted
                predicted_taken  := predicted_taken
                instruction      := instruction
                // valid            := Mux(io.in.valid, valid, VecInit(Seq.fill(4)(false.B)))
                valid                := valid
                instruction_reserved := io.in.instruction
                state                := sPause
            }.otherwise {
                PC               := PC_next
                PC_next          := io.in.PC_next
                offset_predicted := io.in.offset_predicted
                predicted_taken  := io.in.predicted_taken
                instruction      := io.in.instruction
                valid            := valid_normal
            }
        }
        is(sPause) {
            when(io.in.clear || reset.asBool || reFetch_mask) {
                PC_next              := 0.U
                offset_predicted     := 0.U
                predicted_taken      := false.B
                instruction          := VecInit(Seq.fill(4)(0.U(32.W)))
                instruction_reserved := VecInit(Seq.fill(4)(0.U(32.W)))
                valid                := VecInit(Seq.fill(4)(false.B))
                when(io.in.clear || reset.asBool) {
                    state := sClear
                }.otherwise {
                    state := sNormal
                }
            }.otherwise {
                when(!io.in.pause) {
                    PC_next          := io.in.PC_next
                    PC               := PC_next
                    offset_predicted := io.in.offset_predicted
                    predicted_taken  := io.in.predicted_taken
                    valid            := valid_normal
                    instruction      := instruction_reserved
                    state            := sNormal
                }.otherwise {
                    PC_next              := PC_next
                    PC                   := PC
                    offset_predicted     := offset_predicted
                    predicted_taken      := predicted_taken
                    instruction          := instruction
                    instruction_reserved := instruction_reserved
                    valid                := valid
                }
            }
        }
        is(sClear) {
            PC                   := PC_next
            PC_next              := io.in.PC_next
            offset_predicted     := io.in.offset_predicted
            predicted_taken      := io.in.predicted_taken
            instruction          := VecInit(Seq.fill(4)(0.U(32.W)))
            instruction_reserved := VecInit(Seq.fill(4)(0.U(32.W)))
            valid                := VecInit(Seq.fill(4)(false.B))
            when(!io.in.clear && !reset.asBool) {
                state := sNormal
            }
        }
    }

    // 预取指阶段的PC比数据流提前一个周期，使用RegNext来延迟
    /*
    when(io.in.clear || reset.asBool || !io.in.valid) {
        // PC          := 0.U
        // PC_next     := 0.U
        instruction := VecInit(Seq.fill(4)(0.U(32.W)))
        valid       := VecInit(Seq.fill(4)(false.B))
    }.elsewhen(io.in.pause) {
        PC          := PC
        PC_next     := PC_next
        instruction := instruction
        valid       := valid
        // valid := VecInit(Seq.fill(4)(false.B))
    }.otherwise {
        PC_next     := io.in.PC_next
        instruction := io.in.instruction
    }
     */

    io.out.PC               := PC
    io.out.PC_next          := PC_next
    io.out.valid            := valid
    io.out.instruction      := instruction
    io.out.offset_predicted := offset_predicted
    io.out.predicted_taken  := predicted_taken
}

object Pipline_F_D extends App {
    ChiselStage.emitSystemVerilogFile(
      new Pipline_F_D(),
      Array("--target-dir", "generated")
    )
}
