// Designed by Kyle. 2025-03-29 16:14
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import chisel3.util.ShiftRegister
import CONSTANTS.EXECUTION_CYCLE
import CONSTANTS.CONFIG.debugSignal

/*
    （前递）广播与唤醒控制器
    MUX_control：0 -> immediate (uiL/R), 1 -> Register (srcL/R), [2-5] -> broadcast[0-3]
 */
class BroadcastWakeupControllerNew2(nPsyReg: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val ALU0 = new Bundle {
                val srcL  = Input(UInt(log2Ceil(nPsyReg).W))
                val valL  = Input(Bool())
                val srcL2 = Input(UInt(log2Ceil(nPsyReg).W))
                val valL2 = Input(Bool())
                val srcR  = Input(UInt(log2Ceil(nPsyReg).W))
                val valR  = Input(Bool())
                val srcR2 = Input(UInt(log2Ceil(nPsyReg).W))
                val valR2 = Input(Bool())
                val dest  = Input(UInt(log2Ceil(nPsyReg).W))
                val dest2 = Input(UInt(log2Ceil(nPsyReg).W))
                val dest3 = Input(UInt(log2Ceil(nPsyReg).W))
            }
            val ALU1 = new Bundle {
                val srcL  = Input(UInt(log2Ceil(nPsyReg).W))
                val valL  = Input(Bool())
                val srcL2 = Input(UInt(log2Ceil(nPsyReg).W))
                val valL2 = Input(Bool())
                val srcR  = Input(UInt(log2Ceil(nPsyReg).W))
                val valR  = Input(Bool())
                val srcR2 = Input(UInt(log2Ceil(nPsyReg).W))
                val valR2 = Input(Bool())
                val dest  = Input(UInt(log2Ceil(nPsyReg).W))
                val dest2 = Input(UInt(log2Ceil(nPsyReg).W))
                val dest3 = Input(UInt(log2Ceil(nPsyReg).W))
            }
            val MEM = new Bundle {
                val srcL  = Input(UInt(log2Ceil(nPsyReg).W))
                val valL  = Input(Bool())
                val srcL2 = Input(UInt(log2Ceil(nPsyReg).W))
                val valL2 = Input(Bool())
                val srcR  = Input(UInt(log2Ceil(nPsyReg).W))
                val valR  = Input(Bool())
                val srcR2 = Input(UInt(log2Ceil(nPsyReg).W))
                val valR2 = Input(Bool())
                val dest  = Input(UInt(log2Ceil(nPsyReg).W))
                val dest2 = Input(UInt(log2Ceil(nPsyReg).W))
                val dest3 = Input(UInt(log2Ceil(nPsyReg).W))
            }
            val MUL = new Bundle {
                val srcL  = Input(UInt(log2Ceil(nPsyReg).W))
                val srcL2 = Input(UInt(log2Ceil(nPsyReg).W))
                val srcR  = Input(UInt(log2Ceil(nPsyReg).W))
                val srcR2 = Input(UInt(log2Ceil(nPsyReg).W))
                val dest  = Input(UInt(log2Ceil(nPsyReg).W))
                val dest2 = Input(UInt(log2Ceil(nPsyReg).W))
                val dest3 = Input(UInt(log2Ceil(nPsyReg).W))
            }
            val issueValid = Input(Vec(4, Bool())) // 发射有效信号 (来自IssueQueue)
            val cacheMiss  = Input(Bool())
        }
        val out = new Bundle {
            val wake_up_valid = Output(Vec(4, Bool()))                    // 唤醒使能
            val wake_up_regs  = Output(Vec(4, UInt(log2Ceil(nPsyReg).W))) // 唤醒寄存器

            val MUX_control = new Bundle {
                val MUX_ALU_0L = Output(UInt(3.W))
                val MUX_ALU_0R = Output(UInt(3.W))
                val MUX_ALU_1L = Output(UInt(3.W))
                val MUX_ALU_1R = Output(UInt(3.W))
                val MUX_MEM_L  = Output(UInt(3.W))
                val MUX_MEM_R  = Output(UInt(3.W))
                val MUX_MUL_L  = Output(UInt(3.W))
                val MUX_MUL_R  = Output(UInt(3.W))
            }
            val stallALU0 = Output(Bool())
            val stallALU1 = Output(Bool())
            val stallMEM  = Output(Bool())
            val stallMUL  = Output(Bool())

            val muxStallALU0L = Output(Bool())
            val muxStallALU0R = Output(Bool())
            val muxStallALU1L = Output(Bool())
            val muxStallALU1R = Output(Bool())
            val muxStallMemL  = Output(Bool())
            val muxStallMemR  = Output(Bool())
            val muxStallMULL  = Output(Bool())
            val muxStallMULR  = Output(Bool())
        }
    })

    val stallALU0 = Wire(Bool())
    val stallALU1 = Wire(Bool())
    val stallMEM  = Wire(Bool())
    val stallMUL  = Wire(Bool())

    /* ------------ Wake Up Control ------------ */
    // ALU指令，立即唤醒
    io.out.wake_up_valid(0) := Mux(stallALU0, false.B, io.in.issueValid(0))
    io.out.wake_up_regs(0)  := Mux(stallALU0, 0.U, io.in.ALU0.dest)
    io.out.wake_up_valid(1) := Mux(stallALU1, false.B, io.in.issueValid(1))
    io.out.wake_up_regs(1)  := Mux(stallALU1, 0.U, io.in.ALU1.dest)

    io.out.wake_up_valid(2) := Mux(stallMEM, false.B, io.in.issueValid(2))
    io.out.wake_up_regs(2)  := Mux(stallMEM, 0.U, io.in.MEM.dest)

    // MEM指令，延迟 MEM - 1 周期唤醒
    // io.out.wake_up_valid(2) := ShiftRegister(io.in.issueValid(2), EXECUTION_CYCLE.MEM - 1)
    // io.out.wake_up_regs(2)  := ShiftRegister(io.in.regMEM.dest, EXECUTION_CYCLE.MEM - 1)
    // MUL指令，延迟 MUL - 1 周期唤醒
    val mul_wake_up_valid =
        ShiftRegister(io.in.issueValid(3), EXECUTION_CYCLE.MUL - 1, !stallMUL)
    val mul_wake_up_regs =
        ShiftRegister(io.in.MUL.dest, EXECUTION_CYCLE.MUL - 1, !stallMUL)

    io.out.wake_up_valid(3) := Mux(stallMUL, false.B, mul_wake_up_valid)
    io.out.wake_up_regs(3)  := Mux(stallMUL, 0.U, mul_wake_up_regs)

    /* ------------ Broadcast MUX Control ------------ */
    // MUX_control 发出的控制信号在下一周期与（从寄存器读出的）数据流同步
    // Helper function to determine forwarding
    def determineForwarding(src: UInt, valid: Bool): UInt = {
        val result = Wire(UInt(3.W))

        // Default: If valid, use register file (1.U), else use immediate (0.U)
        result := Mux(valid, 1.U, 0.U)

        when(src === io.in.ALU0.dest3 && src =/= 0.U && valid) {
            result := 2.U // Forward from ALU0
        }.elsewhen(src === io.in.ALU1.dest3 && src =/= 0.U && valid) {
            result := 3.U // Forward from ALU1
        }.elsewhen(src === io.in.MEM.dest3 && src =/= 0.U && valid) {
            result := 4.U // Forward from MEM
        }.elsewhen(src === io.in.MUL.dest3 && src =/= 0.U && valid) {
            result := 5.U // Forward from MUL
        }

        result
    }

    io.out.MUX_control.MUX_ALU_0L := determineForwarding(
      io.in.ALU0.srcL2,
      io.in.ALU0.valL2
    )
    io.out.MUX_control.MUX_ALU_0R := determineForwarding(
      io.in.ALU0.srcR2,
      io.in.ALU0.valR2
    )
    io.out.MUX_control.MUX_ALU_1L := determineForwarding(
      io.in.ALU1.srcL2,
      io.in.ALU1.valL2
    )
    io.out.MUX_control.MUX_ALU_1R := determineForwarding(
      io.in.ALU1.srcR2,
      io.in.ALU1.valR2
    )
    io.out.MUX_control.MUX_MEM_L := determineForwarding(io.in.MEM.srcL2, io.in.MEM.valL2)
    io.out.MUX_control.MUX_MEM_R := determineForwarding(io.in.MEM.srcR2, io.in.MEM.valR2)
    io.out.MUX_control.MUX_MUL_L := determineForwarding(io.in.MUL.srcL2, true.B)
    io.out.MUX_control.MUX_MUL_R := determineForwarding(io.in.MUL.srcR2, true.B)

    val directStallALU0L = WireInit(false.B)
    val directStallALU0R = WireInit(false.B)
    val directStallALU1L = WireInit(false.B)
    val directStallALU1R = WireInit(false.B)
    val directStallMEM_L = WireInit(false.B)
    val directStallMEM_R = WireInit(false.B)
    val directStallMULL  = WireInit(false.B)
    val directStallMULR  = WireInit(false.B)

    val counterALU0 = VecInit(Seq.fill(2)(0.U(5.W)))
    val counterALU1 = VecInit(Seq.fill(2)(0.U(5.W)))
    val counterMEM  = VecInit(Seq.fill(2)(0.U(5.W)))
    val counterMUL  = VecInit(Seq.fill(2)(0.U(5.W)))

    val directStallALU0 =
        io.in.cacheMiss && (io.out.MUX_control.MUX_ALU_0L === 4.U || io.out.MUX_control.MUX_ALU_0R === 4.U)
    val directStallALU1 =
        io.in.cacheMiss && (io.out.MUX_control.MUX_ALU_1L === 4.U || io.out.MUX_control.MUX_ALU_1R === 4.U)
    val directStallMEM = io.in.cacheMiss
    val directStallMUL =
        io.in.cacheMiss && (io.out.MUX_control.MUX_MUL_L === 4.U || io.out.MUX_control.MUX_MUL_R === 4.U)

    debugSignal(directStallALU0)
    debugSignal(directStallALU1)
    debugSignal(directStallMEM)
    debugSignal(directStallMUL)

    stallALU0 := directStallALU0 || (io.in.issueValid(0) &&
        ((io.in.ALU0.srcL === io.in.ALU1.dest2 && directStallALU1 && io.in.ALU0.valL) ||
            (io.in.ALU0.srcR === io.in.ALU1.dest2 && directStallALU1 && io.in.ALU0.valR) ||
            (io.in.ALU0.srcL === io.in.MEM.dest2 && directStallMEM && io.in.ALU0.valL) ||
            (io.in.ALU0.srcR === io.in.MEM.dest2 && directStallMEM && io.in.ALU0.valR) ||
            (io.in.ALU0.srcL === io.in.MUL.dest2 && directStallMULL && io.in.ALU0.valL) ||
            (io.in.ALU0.srcR === io.in.MUL.dest2 && directStallMULL && io.in.ALU0.valR)))

    stallALU1 := directStallALU1 || (io.in.issueValid(1) &&
        ((io.in.ALU1.srcL === io.in.ALU0.dest2 && directStallALU0 && io.in.ALU1.valL) ||
            (io.in.ALU1.srcR === io.in.ALU0.dest2 && directStallALU0 && io.in.ALU1.valR) ||
            (io.in.ALU1.srcL === io.in.MEM.dest2 && directStallMEM && io.in.ALU1.valL) ||
            (io.in.ALU1.srcR === io.in.MEM.dest2 && directStallMEM && io.in.ALU1.valR) ||
            (io.in.ALU1.srcL === io.in.MUL.dest2 && directStallMULL && io.in.ALU1.valL) ||
            (io.in.ALU1.srcR === io.in.MUL.dest2 && directStallMULL && io.in.ALU1.valR)))

    stallMEM := directStallMEM

    stallMUL := directStallMUL || (io.in.issueValid(3) &&
        ((io.in.MUL.srcL === io.in.ALU0.dest2 && directStallALU0) ||
            (io.in.MUL.srcR === io.in.ALU0.dest2 && directStallALU0) ||
            (io.in.MUL.srcL === io.in.ALU1.dest2 && directStallALU1) ||
            (io.in.MUL.srcR === io.in.ALU1.dest2 && directStallALU1) ||
            (io.in.MUL.srcL === io.in.MEM.dest2 && directStallMEM) ||
            (io.in.MUL.srcR === io.in.MEM.dest2 && directStallMEM)))

    io.out.stallALU0 := stallALU0
    io.out.stallALU1 := stallALU1
    io.out.stallMEM  := stallMEM
    io.out.stallMUL  := stallMUL

    // No longer needed
    io.out.muxStallALU0L := false.B
    io.out.muxStallALU0R := false.B
    io.out.muxStallALU1L := false.B
    io.out.muxStallALU1R := false.B
    io.out.muxStallMemL  := false.B
    io.out.muxStallMemR  := false.B
    io.out.muxStallMULL  := false.B
    io.out.muxStallMULR  := false.B
}

object BroadcastWakeupControllerNew2 extends App {
    ChiselStage.emitSystemVerilogFile(
      new BroadcastWakeupController(nPsyReg = 64),
      Array("--target-dir", "generated")
    )
}
