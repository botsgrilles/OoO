// Designed by Kyle. 2025-03-29 16:14
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import chisel3.util.ShiftRegister
import CONSTANTS.EXECUTION_CYCLE

/*
    （前递）广播与唤醒控制器
    MUX_control：0 -> immediate (uiL/R), 1 -> Register (srcL/R), [2-5] -> broadcast[0-3]
 */
class BroadcastWakeupControllerNew(nPsyReg: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val regALU0 = new Bundle {
                val srcL = Input(UInt(log2Ceil(nPsyReg).W))
                val valL = Input(Bool())
                val srcR = Input(UInt(log2Ceil(nPsyReg).W))
                val valR = Input(Bool())
                val dest = Input(UInt(log2Ceil(nPsyReg).W))
            }
            val regALU1 = new Bundle {
                val srcL = Input(UInt(log2Ceil(nPsyReg).W))
                val valL = Input(Bool())
                val srcR = Input(UInt(log2Ceil(nPsyReg).W))
                val valR = Input(Bool())
                val dest = Input(UInt(log2Ceil(nPsyReg).W))
            }
            val regMEM = new Bundle {
                val srcL = Input(UInt(log2Ceil(nPsyReg).W))
                val valL = Input(Bool())
                val srcR = Input(UInt(log2Ceil(nPsyReg).W))
                val valR = Input(Bool())
                val dest = Input(UInt(log2Ceil(nPsyReg).W))
            }
            val regMUL = new Bundle {
                val srcL = Input(UInt(log2Ceil(nPsyReg).W))
                val srcR = Input(UInt(log2Ceil(nPsyReg).W))
                val dest = Input(UInt(log2Ceil(nPsyReg).W))
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
    io.out.wake_up_regs(0)  := Mux(stallALU0, 0.U, io.in.regALU0.dest)
    io.out.wake_up_valid(1) := Mux(stallALU1, false.B, io.in.issueValid(1))
    io.out.wake_up_regs(1)  := Mux(stallALU1, 0.U, io.in.regALU1.dest)

    io.out.wake_up_valid(2) := Mux(stallMEM, false.B, io.in.issueValid(2))
    io.out.wake_up_regs(2)  := Mux(stallMEM, 0.U, io.in.regMEM.dest)

    // MEM指令，延迟 MEM - 1 周期唤醒
    // io.out.wake_up_valid(2) := ShiftRegister(io.in.issueValid(2), EXECUTION_CYCLE.MEM - 1)
    // io.out.wake_up_regs(2)  := ShiftRegister(io.in.regMEM.dest, EXECUTION_CYCLE.MEM - 1)
    // MUL指令，延迟 MUL - 1 周期唤醒
    val mul_wake_up_valid =
        ShiftRegister(io.in.issueValid(3), EXECUTION_CYCLE.MUL - 1, !stallMUL)
    val mul_wake_up_regs =
        ShiftRegister(io.in.regMUL.dest, EXECUTION_CYCLE.MUL - 1, !stallMUL)

    io.out.wake_up_valid(3) := Mux(stallMUL, false.B, mul_wake_up_valid)
    io.out.wake_up_regs(3)  := Mux(stallMUL, 0.U, mul_wake_up_regs)

    /* ------------ Broadcast MUX Control ------------ */
    // Default: Select from Register file
    io.out.MUX_control.MUX_ALU_0L := RegNext(Mux(io.in.regALU0.valL, 1.U, 0.U))
    io.out.MUX_control.MUX_ALU_0R := RegNext(Mux(io.in.regALU0.valR, 1.U, 0.U))
    io.out.MUX_control.MUX_ALU_1L := RegNext(Mux(io.in.regALU1.valL, 1.U, 0.U))
    io.out.MUX_control.MUX_ALU_1R := RegNext(Mux(io.in.regALU1.valR, 1.U, 0.U))
    io.out.MUX_control.MUX_MEM_L  := 1.U
    io.out.MUX_control.MUX_MEM_R  := 1.U
    io.out.MUX_control.MUX_MUL_L  := 1.U
    io.out.MUX_control.MUX_MUL_R  := 1.U

    // Pipeline registers for tracking in-flight operations
    val alu0DestReg = RegInit(0.U(log2Ceil(nPsyReg).W))
    val alu1DestReg = RegInit(0.U(log2Ceil(nPsyReg).W))
    val memDestReg  = RegInit(0.U(log2Ceil(nPsyReg).W))
    val mulDestReg = RegInit(
      VecInit(Seq.fill(EXECUTION_CYCLE.MUL)(0.U(log2Ceil(nPsyReg).W)))
    )

    // MUX_control 发出的控制信号在下一周期与（从寄存器读出的）数据流同步
    // Helper function to determine forwarding
    def determineForwarding(src: UInt, valid: Bool): UInt = {
        val result = Wire(UInt(3.W))

        // Default: If valid, use register file (1.U), else use immediate (0.U)
        result := Mux(valid, 1.U, 0.U)

        when(src === alu0DestReg && src =/= 0.U && valid) {
            result := 2.U // Forward from ALU0
        }.elsewhen(src === alu1DestReg && src =/= 0.U && valid) {
            result := 3.U // Forward from ALU1
        }.elsewhen(src === memDestReg && src =/= 0.U && valid) {
            result := 4.U // Forward from MEM
        }.elsewhen(src === mulDestReg(EXECUTION_CYCLE.MUL - 1) && src =/= 0.U && valid) {
            result := 5.U // Forward from MUL
        }

        result
    }

    val reg_MUX_ALU_0L = RegInit(0.U(3.W))
    val reg_MUX_ALU_0R = RegInit(0.U(3.W))
    val reg_MUX_ALU_1L = RegInit(0.U(3.W))
    val reg_MUX_ALU_1R = RegInit(0.U(3.W))
    val reg_MUX_MEM_L  = RegInit(0.U(3.W))
    val reg_MUX_MEM_R  = RegInit(0.U(3.W))
    val reg_MUX_MUL_L  = RegInit(0.U(3.W))
    val reg_MUX_MUL_R  = RegInit(0.U(3.W))

    val regStallALU0 = RegInit(false.B)
    val regStallALU1 = RegInit(false.B)
    val regStallMEM  = RegInit(false.B)
    val regStallMUL  = RegInit(false.B)

    val MUX_ALU_0L = determineForwarding(io.in.regALU0.srcL, io.in.regALU0.valL)
    val MUX_ALU_0R = determineForwarding(io.in.regALU0.srcR, io.in.regALU0.valR)
    val MUX_ALU_1L = determineForwarding(io.in.regALU1.srcL, io.in.regALU1.valL)
    val MUX_ALU_1R = determineForwarding(io.in.regALU1.srcR, io.in.regALU1.valR)
    val MUX_MEM_L  = determineForwarding(io.in.regMEM.srcL, io.in.regMEM.valL)
    val MUX_MEM_R  = determineForwarding(io.in.regMEM.srcR, io.in.regMEM.valR)
    val MUX_MUL_L  = determineForwarding(io.in.regMUL.srcL, true.B) // MUL always valid
    val MUX_MUL_R  = determineForwarding(io.in.regMUL.srcR, true.B)

    reg_MUX_ALU_0L := Mux(stallALU0, reg_MUX_ALU_0L, MUX_ALU_0L)
    reg_MUX_ALU_0R := Mux(stallALU0, reg_MUX_ALU_0R, MUX_ALU_0R)
    reg_MUX_ALU_1L := Mux(stallALU1, reg_MUX_ALU_1L, MUX_ALU_1L)
    reg_MUX_ALU_1R := Mux(stallALU1, reg_MUX_ALU_1R, MUX_ALU_1R)
    reg_MUX_MEM_L  := Mux(stallMEM, reg_MUX_MEM_L, MUX_MEM_L)
    reg_MUX_MEM_R  := Mux(stallMEM, reg_MUX_MEM_R, MUX_MEM_R)
    reg_MUX_MUL_L  := Mux(stallMUL, reg_MUX_MUL_L, MUX_MUL_L)
    reg_MUX_MUL_R  := Mux(stallMUL, reg_MUX_MUL_R, MUX_MUL_R)

    val stallALU0L = WireInit(false.B)
    val stallALU0R = WireInit(false.B)
    val stallALU1L = WireInit(false.B)
    val stallALU1R = WireInit(false.B)
    val stallMEM_L = WireInit(false.B)
    val stallMEM_R = WireInit(false.B)
    val stallMULL  = WireInit(false.B)
    val stallMULR  = WireInit(false.B)

    val counterALU0 = RegInit(VecInit(Seq.fill(2)(0.U(5.W))))
    val counterALU1 = RegInit(VecInit(Seq.fill(2)(0.U(5.W))))
    val counterMEM  = RegInit(VecInit(Seq.fill(2)(0.U(5.W))))
    val counterMUL  = RegInit(VecInit(Seq.fill(2)(0.U(5.W))))

    // 区分左右的stall信号可理解为：“因左/右侧发生依赖而stall”，用来判断执行级是否需要锁存前递而来的信号
    // 暂停条件：1. 本周期依赖访存级，并且访存级被暂停了（最基本的）
    //         2. 当前指令依赖的其他通路之前被暂停过，要求暂停相同的周期；但如果依赖的数据流上一周期没暂停，计数器被更新，无法直接比较，但这个时候必不会暂停本数据流
    // 所有暂停信号只在执行级发送

    stallALU0L := (reg_MUX_ALU_0L === 4.U && io.in.cacheMiss) ||
        (reg_MUX_ALU_0L === 3.U && counterALU0(1) < counterALU1(1) || !regStallALU0) ||
        (reg_MUX_ALU_0L === 4.U && counterALU0(1) < counterMEM(1) || !regStallALU1) ||
        (reg_MUX_ALU_0L === 5.U && counterALU0(1) < counterMUL(1) || !regStallMUL)

    stallALU0R := (reg_MUX_ALU_0R === 4.U && io.in.cacheMiss) ||
        (reg_MUX_ALU_0R === 2.U && counterALU0(1) < counterALU1(1) || !regStallALU0) ||
        (reg_MUX_ALU_0R === 4.U && counterALU0(1) < counterMEM(1) || !regStallALU1) ||
        (reg_MUX_ALU_0R === 5.U && counterALU0(1) < counterMUL(1) || !regStallMUL)

    stallALU1L := (reg_MUX_ALU_1L === 4.U && io.in.cacheMiss) ||
        (reg_MUX_ALU_1L === 2.U && counterALU1(1) < counterALU0(1) || !regStallALU1) ||
        (reg_MUX_ALU_1L === 3.U && counterALU1(1) < counterMEM(1) || !regStallALU0) ||
        (reg_MUX_ALU_1L === 5.U && counterALU1(1) < counterMUL(1) || !regStallMUL)

    stallALU1R := (reg_MUX_ALU_1R === 4.U && io.in.cacheMiss) ||
        (reg_MUX_ALU_1R === 2.U && counterALU1(1) < counterALU0(1) || !regStallALU1) ||
        (reg_MUX_ALU_1R === 3.U && counterALU1(1) < counterMEM(1) || !regStallALU0) ||
        (reg_MUX_ALU_1R === 4.U && counterALU1(1) < counterMUL(1) || !regStallMUL)

    stallMEM_L := io.in.cacheMiss && (reg_MUX_MEM_L === 2.U || reg_MUX_MEM_L === 3.U || reg_MUX_MEM_L === 5.U)
    stallMEM_R := io.in.cacheMiss && (reg_MUX_MEM_R === 2.U || reg_MUX_MEM_R === 3.U || reg_MUX_MEM_R === 5.U)

    stallMULL := (reg_MUX_MUL_L === 4.U && io.in.cacheMiss) ||
        (reg_MUX_MUL_L === 2.U && counterMUL(1) < counterALU0(1) || !regStallMEM) ||
        (reg_MUX_MUL_L === 3.U && counterMUL(1) < counterALU1(1) || !regStallALU0) ||
        (reg_MUX_MUL_L === 4.U && counterMUL(1) < counterMEM(1) || !regStallMUL)

    stallMULR := (reg_MUX_MUL_R === 4.U && io.in.cacheMiss) ||
        (reg_MUX_MUL_R === 2.U && counterMUL(1) < counterALU0(1) || !regStallMEM) ||
        (reg_MUX_MUL_R === 3.U && counterMUL(1) < counterALU1(1) || !regStallALU0) ||
        (reg_MUX_MUL_R === 4.U && counterMUL(1) < counterMEM(1) || !regStallMUL)

    stallALU0 := stallALU0L || stallALU0R
    stallALU1 := stallALU1L || stallALU1R
    stallMEM  := io.in.cacheMiss // cacheMiss是根本原因，若不miss，没有其他原因导致访存级被暂停
    stallMUL  := stallMULL || stallMULR

    when(stallALU0L || stallALU0R) {
        counterALU0(0) := counterALU0(0) + 1.U
        counterALU0(1) := counterALU0(1) + 1.U
    }.otherwise {
        counterALU0(0) := 0.U
        counterALU0(1) := counterALU0(0)
    }

    when(stallALU1L || stallALU1R) {
        counterALU1(0) := counterALU1(0) + 1.U
        counterALU1(1) := counterALU1(1) + 1.U
    }.otherwise {
        counterALU1(0) := 0.U
        counterALU1(1) := counterALU1(0)
    }

    when(stallMEM_L || stallMEM_R) {
        counterMEM(0) := counterMEM(0) + 1.U
        counterMEM(1) := counterMEM(1) + 1.U
    }.otherwise {
        counterMEM(0) := 0.U
        counterMEM(1) := counterMEM(0)
    }

    when(stallMULL || stallMULR) {
        counterMUL(0) := counterMUL(0) + 1.U
        counterMUL(1) := counterMUL(1) + 1.U
    }.otherwise {
        counterMUL(0) := 0.U
        counterMUL(1) := counterMUL(0)
    }

    // Update tracking registers
    alu0DestReg := Mux(stallALU0, alu0DestReg, io.in.regALU0.dest)
    alu1DestReg := Mux(stallALU1, alu1DestReg, io.in.regALU1.dest)
    memDestReg  := Mux(stallMEM, memDestReg, io.in.regMEM.dest)

    for (i <- 1 until EXECUTION_CYCLE.MUL) {
        mulDestReg(i) := Mux(stallMUL, mulDestReg(i), mulDestReg(i - 1))
    }
    mulDestReg(0) := Mux(stallMUL, mulDestReg(0), io.in.regMUL.dest)

    io.out.stallALU0 := stallALU0
    io.out.stallALU1 := stallALU1
    io.out.stallMEM  := stallMEM
    io.out.stallMUL  := stallMUL

    io.out.muxStallALU0L := !stallALU0L && stallALU0R
    io.out.muxStallALU0R := !stallALU0R && stallALU0L
    io.out.muxStallALU1L := !stallALU1L && stallALU1R
    io.out.muxStallALU1R := !stallALU1R && stallALU1L
    io.out.muxStallMemL  := stallMEM && reg_MUX_MEM_L =/= 4.U
    io.out.muxStallMemR  := stallMEM && reg_MUX_MEM_R =/= 4.U
    io.out.muxStallMULL  := !stallMULL && stallMULR
    io.out.muxStallMULR  := !stallMULR && stallMULL
}

object BroadcastWakeupControllerNew extends App {
    ChiselStage.emitSystemVerilogFile(
      new BroadcastWakeupController(nPsyReg = 64),
      Array("--target-dir", "generated")
    )
}
