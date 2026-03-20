// Designed by Kyle. 2025-04-17 13:45
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class Pipline_RN_WR(nPsyReg: Int) extends Module {
    val defaultBundle = new Bundle {
        val srcL        = UInt(log2Ceil(nPsyReg).W) // 源操作数1
        val valL        = Bool()
        val srcR        = UInt(log2Ceil(nPsyReg).W) // 源操作数2
        val valR        = Bool()
        val dest        = UInt(log2Ceil(nPsyReg).W) // 目的操作数
        val dest_origin = UInt(5.W)
        val dest_old    = UInt(log2Ceil(nPsyReg).W)
        val uiL         = UInt(32.W)                // 立即数1
        val uiR         = UInt(32.W)                // 立即数2
        val branch_inst = UInt(4.W)                 // 跳转类型：是否跳转 (1bit) + 类型 (3bit)
        val FU          = UInt(2.W)                 // 功能单元
        val ALU_OP      = UInt(4.W)                 // ALU操作
        val mem_size    = UInt(2.W)
        val reg_write   = Bool()
        val mem_write   = Bool()
        val mem_read    = Bool()
    }

    val io = IO(new Bundle {
        val in = new Bundle {
            val pause            = Input(Bool())
            val clear            = Input(Bool())
            val valid            = Input(Vec(4, Bool()))
            val PC               = Input(UInt(32.W))
            val PC_next          = Input(UInt(32.W))
            val PC_decode        = Input(UInt(32.W))
            val decode_PC_set    = Input(Bool())
            val predicted_taken  = Input(Bool()) // 分支预测器的预测结果
            val offset_predicted = Input(UInt(2.W))
            val default          = Input(Vec(4, defaultBundle))

        }
        val out = new Bundle {
            val PC               = Output(Vec(4, UInt(32.W)))
            val PC_next          = Output(Vec(4, UInt(32.W)))
            val PC_decode        = Output(Vec(4, UInt(32.W)))
            val decode_PC_set    = Output(Vec(4, Bool()))
            val predicted_taken  = Output(Vec(4, Bool())) // 分支预测器的预测结果
            val offset_predicted = Output(Vec(4, UInt(2.W)))
            val valid            = Output(Vec(4, Bool())) // Add valid output
            val default          = Output(Vec(4, defaultBundle))
        }
    })

    val reg_PC               = RegInit(0.U(32.W))
    val reg_PC_next          = RegInit(0.U(32.W))
    val reg_PC_decode        = RegInit(0.U(32.W))
    val reg_decode_PC_set    = RegInit(false.B)
    val reg_offset_predicted = RegInit(0.U(2.W))
    val reg_predicted_taken  = RegInit(VecInit(Seq.fill(4)(false.B)))
    val reg_default          = RegInit(VecInit(Seq.fill(4)(0.U.asTypeOf(defaultBundle))))
    val reg_valid            = RegInit(VecInit(Seq.fill(4)(false.B)))

    when(io.in.clear || reset.asBool) {
        reg_PC               := 0.U
        reg_PC_next          := 0.U
        reg_PC_decode        := 0.U
        reg_decode_PC_set    := false.B
        reg_offset_predicted := 0.U
        for (i <- 0 until 4) {
            // Reset reg_default elements using the Bundle type
            reg_default(i)         := 0.U.asTypeOf(defaultBundle)
            reg_valid(i)           := false.B // Clear valid bits
            reg_predicted_taken(i) := false.B // Clear predicted_taken bits
        }
    }.elsewhen(io.in.pause) {
        reg_valid := reg_valid
        // reg_valid := Seq.fill(4)(false.B)
        // Do nothing, keep the current values
    }.otherwise {
        reg_PC               := io.in.PC
        reg_PC_next          := io.in.PC_next
        reg_PC_decode        := io.in.PC_decode
        reg_decode_PC_set    := io.in.decode_PC_set
        reg_offset_predicted := io.in.offset_predicted
        reg_default          := io.in.default
        reg_valid            := io.in.valid // Pass valid bits through
    }

    for (i <- 0 until 4) {
        io.out.PC(i)               := Cat(reg_PC(31, 4), 0.U(4.W)) + (4 * i).U
        io.out.PC_next(i)          := reg_PC_next
        io.out.PC_decode(i)        := reg_PC_decode
        io.out.decode_PC_set(i)    := reg_decode_PC_set
        io.out.offset_predicted(i) := reg_offset_predicted
    }

    // when(io.in.predicted_taken && !io.in.pause) {
    when(io.in.predicted_taken && !io.in.pause) {
        switch(io.in.offset_predicted) {
            is(0.U) {
                reg_predicted_taken := VecInit(Seq(true.B, false.B, false.B, false.B))
            }
            is(1.U) {
                reg_predicted_taken := VecInit(Seq(false.B, true.B, false.B, false.B))
            }
            is(2.U) {
                reg_predicted_taken := VecInit(Seq(false.B, false.B, true.B, false.B))
            }
            is(3.U) {
                reg_predicted_taken := VecInit(Seq(false.B, false.B, false.B, true.B))
            }
        }
    }
    .elsewhen(io.in.pause){
        reg_predicted_taken := reg_predicted_taken
    }
    .otherwise{
        reg_predicted_taken := VecInit(Seq(false.B, false.B, false.B, false.B))
    }

    io.out.default := reg_default
    io.out.valid   := reg_valid // Connect registered valid to output

    io.out.predicted_taken := reg_predicted_taken
}

object Pipline_RN_WR extends App {
    ChiselStage.emitSystemVerilogFile(
      new Pipline_RN_WR(nPsyReg = 64),
      Array("--target-dir", "generated")
    )
}
