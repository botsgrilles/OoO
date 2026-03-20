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

object broadcastCode {
    val alu0 = "b0001".U
    val alu1 = "b0010".U
    val mem  = "b0100".U
    val mul  = "b1000".U
}

class BroadcastWakeupController(nPsyReg: Int) extends Module {
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
            val infoALU0 = Input(new regsInfo(nPsyReg))
            val infoALU1 = Input(new regsInfo(nPsyReg))
            val infoMEM  = Input(new regsInfo(nPsyReg))
            val infoMUL  = Input(new regsInfo(nPsyReg))
            val issueValid = Input(Vec(4, Bool())) // 发射有效信号 (来自IssueQueue)
            val validWire = Input(Vec(4, Bool()))
            val cacheMiss  = Input(Bool())
        }
        val out = new Bundle {
            val MUX_control = new Bundle {
                val MUX_ALU_0L = Output(UInt(6.W))
                val MUX_ALU_0R = Output(UInt(6.W))
                val MUX_ALU_1L = Output(UInt(6.W))
                val MUX_ALU_1R = Output(UInt(6.W))
                val MUX_MEM_L  = Output(UInt(6.W))
                val MUX_MEM_R  = Output(UInt(6.W))
                val MUX_MUL_L  = Output(UInt(6.W))
                val MUX_MUL_R  = Output(UInt(6.W))
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

    /* ----- Experimental ----- */
    val alu0DestReg_ = RegInit(0.U(log2Ceil(nPsyReg).W))
    val alu0DestReg2_ = RegInit(0.U(log2Ceil(nPsyReg).W))
    val alu1DestReg_ = RegInit(0.U(log2Ceil(nPsyReg).W))
    val alu1DestReg2_ = RegInit(0.U(log2Ceil(nPsyReg).W))
    val memDestReg_  = RegInit(0.U(log2Ceil(nPsyReg).W))
    val memDestReg2_ = RegInit(0.U(log2Ceil(nPsyReg).W))
    val mulDestReg_ = RegInit(
      VecInit(Seq.fill(EXECUTION_CYCLE.MUL)(0.U(log2Ceil(nPsyReg).W)))
    )
    val mulDestReg2_ = RegInit(0.U(log2Ceil(nPsyReg).W))

    val regALU0Info = RegInit(0.U.asTypeOf(new regsInfo(nPsyReg)))
    val regALU1Info = RegInit(0.U.asTypeOf(new regsInfo(nPsyReg)))
    val regMEMInfo  = RegInit(0.U.asTypeOf(new regsInfo(nPsyReg)))
    val regMULInfo  = RegInit(0.U.asTypeOf(new regsInfo(nPsyReg)))

    val stallALU0_ = Wire(Bool())
    val stallALU1_ = Wire(Bool())
    val stallMEM_  = Wire(Bool())
    val stallMUL_  = Wire(Bool())

    val muxStallALU0L_ = Wire(Bool())
    val muxStallALU0R_ = Wire(Bool())
    val muxStallALU1L_ = Wire(Bool())
    val muxStallALU1R_ = Wire(Bool())
    val muxStallMemL_  = Wire(Bool())
    val muxStallMemR_  = Wire(Bool())
    val muxStallMULL_  = Wire(Bool())
    val muxStallMULR_  = Wire(Bool())

    val MUX_ALU_0L_ = WireInit(0.U(6.W))
    val MUX_ALU_0R_ = WireInit(0.U(6.W))
    val MUX_ALU_1L_ = WireInit(0.U(6.W))
    val MUX_ALU_1R_ = WireInit(0.U(6.W))
    val MUX_MEM_L_  = WireInit(0.U(6.W))
    val MUX_MEM_R_  = WireInit(0.U(6.W))
    val MUX_MUL_L_  = WireInit(0.U(6.W))
    val MUX_MUL_R_  = WireInit(0.U(6.W))

    val srcALU0Lreg_ = RegInit(0.U(log2Ceil(nPsyReg).W))
    val srcALU0Rreg_ = RegInit(0.U(log2Ceil(nPsyReg).W))
    val srcALU1Lreg_ = RegInit(0.U(log2Ceil(nPsyReg).W))
    val srcALU1Rreg_ = RegInit(0.U(log2Ceil(nPsyReg).W))
    val srcMEMLreg_  = RegInit(0.U(log2Ceil(nPsyReg).W))
    val srcMEMRreg_  = RegInit(0.U(log2Ceil(nPsyReg).W))
    val srcMULLreg_  = RegInit(0.U(log2Ceil(nPsyReg).W))
    val srcMULRreg_  = RegInit(0.U(log2Ceil(nPsyReg).W))

    val valALU0Lreg_ = RegInit(false.B)
    val valALU0Rreg_ = RegInit(false.B)
    val valALU1Lreg_ = RegInit(false.B)
    val valALU1Rreg_ = RegInit(false.B)
    val valMEMLreg_  = RegInit(false.B)
    val valMEMRreg_  = RegInit(false.B)
    val valMULLreg_  = RegInit(false.B)
    val valMULRreg_  = RegInit(false.B)

    val inFlightRegs = RegInit(VecInit(Seq.fill(nPsyReg)(0.U(2.W))))
    val sourceOfRegs = RegInit(VecInit(Seq.fill(nPsyReg)(0.U(4.W))))

    when(!stallALU0_){
        when(io.in.validWire(0)) {
            inFlightRegs(io.in.infoALU0.dest) := "b01".U
            sourceOfRegs(io.in.infoALU0.dest) := broadcastCode.alu0
        }
        inFlightRegs(alu0DestReg_) := inFlightRegs(alu0DestReg_) << 1.U

        regALU0Info := io.in.infoALU0
        alu0DestReg_ := regALU0Info.dest
        alu0DestReg2_ := alu0DestReg_
        srcALU0Lreg_ := regALU0Info.srcL
        srcALU0Rreg_ := regALU0Info.srcR
        valALU0Lreg_ := regALU0Info.valL
        valALU0Rreg_ := regALU0Info.valR
    }
    inFlightRegs(alu0DestReg2_) := inFlightRegs(alu0DestReg2_) << 1.U

    when(!stallALU1_){
        when(io.in.validWire(1)) {
            inFlightRegs(io.in.infoALU1.dest) := "b01".U
            sourceOfRegs(io.in.infoALU1.dest) := broadcastCode.alu1
        }
        inFlightRegs(alu1DestReg_) := inFlightRegs(alu1DestReg_) << 1.U

        regALU1Info := io.in.infoALU1
        alu1DestReg_ := regALU1Info.dest
        alu1DestReg2_ := alu1DestReg_
        srcALU1Lreg_ := regALU1Info.srcL
        srcALU1Rreg_ := regALU1Info.srcR
        valALU1Lreg_ := regALU1Info.valL
        valALU1Rreg_ := regALU1Info.valR
    }
    inFlightRegs(alu1DestReg2_) := inFlightRegs(alu1DestReg2_) << 1.U

    when(!stallMEM_){
        when(io.in.validWire(2)) {
            inFlightRegs(io.in.infoMEM.dest) := "b01".U
            sourceOfRegs(io.in.infoMEM.dest) := broadcastCode.mem
        }
        inFlightRegs(memDestReg_) := inFlightRegs(memDestReg_) << 1.U
        inFlightRegs(memDestReg2_) := inFlightRegs(memDestReg2_) << 1.U

        regMEMInfo := io.in.infoMEM
        memDestReg_ := regMEMInfo.dest
        memDestReg2_ := memDestReg_
        srcMEMLreg_ := regMEMInfo.srcL
        srcMEMRreg_ := regMEMInfo.srcR
        valMEMLreg_ := regMEMInfo.valL
        valMEMRreg_ := regMEMInfo.valR
    }
    
    when(!stallMUL_){
        when(io.in.validWire(3)) {
            inFlightRegs(io.in.infoMUL.dest) := "b01".U
            sourceOfRegs(io.in.infoMUL.dest) := broadcastCode.mul
        }
        for (i <- 1 until EXECUTION_CYCLE.MUL) {
            mulDestReg_(i) := mulDestReg_(i - 1)
        }

        inFlightRegs(mulDestReg_(EXECUTION_CYCLE.MUL - 1)) := inFlightRegs(mulDestReg_(EXECUTION_CYCLE.MUL - 1)) << 1.U

        regMULInfo := io.in.infoMUL
        mulDestReg_(0) := regMULInfo.dest
        mulDestReg2_ := mulDestReg_(EXECUTION_CYCLE.MUL - 1)

        srcMULLreg_ := regMULInfo.srcL
        srcMULRreg_ := regMULInfo.srcR
        valMULLreg_ := true.B // MUL always valid
        valMULRreg_ := true.B
    }
    inFlightRegs(mulDestReg2_) := inFlightRegs(mulDestReg2_) << 1.U
    
    // val cacheMissMasks_ = Mux(io.in.cacheMiss, UIntToOH(memDestReg2_, nPsyReg), 0.U(nPsyReg.W))

    def determineStall_(src: UInt): Bool = {
        (inFlightRegs(src)(0) || src === memDestReg2_ && io.in.cacheMiss) && src =/= 0.U
    }

    muxStallALU0L_ := determineStall_(srcALU0Lreg_)
    muxStallALU0R_ := determineStall_(srcALU0Rreg_)
    muxStallALU1L_ := determineStall_(srcALU1Lreg_)
    muxStallALU1R_ := determineStall_(srcALU1Rreg_)
    muxStallMemL_  := determineStall_(srcMEMLreg_)
    muxStallMemR_  := determineStall_(srcMEMRreg_)
    muxStallMULL_  := determineStall_(srcMULLreg_)
    muxStallMULR_  := determineStall_(srcMULRreg_)

    stallALU0_ := muxStallALU0L_ || muxStallALU0R_
    stallALU1_ := muxStallALU1L_ || muxStallALU1R_
    stallMEM_  := io.in.cacheMiss || muxStallMemL_ || muxStallMemR_
    stallMUL_  := muxStallMULL_ || muxStallMULR_

    def determineForwarding_(src: UInt, valid: Bool): UInt = {
        val result = Wire(UInt(6.W))
        // when(inFlightRegs(src) === "b100".U && src =/= 0.U && valid) {
        when(inFlightRegs(src)(1) && src =/= 0.U && valid) {
            result := Cat(sourceOfRegs(src), "b00".U(2.W))
        }.otherwise{
            result := Mux(valid, "b000010".U, "b000001".U)
        }

        val debug = Wire(UInt(4.W))
        debug := sourceOfRegs(src)
        result
    }

    MUX_ALU_0L_ := determineForwarding_(srcALU0Lreg_, valALU0Lreg_)
    MUX_ALU_0R_ := determineForwarding_(srcALU0Rreg_, valALU0Rreg_)
    MUX_ALU_1L_ := determineForwarding_(srcALU1Lreg_, valALU1Lreg_)
    MUX_ALU_1R_ := determineForwarding_(srcALU1Rreg_, valALU1Rreg_)
    MUX_MEM_L_  := determineForwarding_(srcMEMLreg_, valMEMLreg_)
    MUX_MEM_R_  := determineForwarding_(srcMEMRreg_, valMEMRreg_)
    MUX_MUL_L_  := determineForwarding_(srcMULLreg_, valMULLreg_)
    MUX_MUL_R_  := determineForwarding_(srcMULRreg_, valMULRreg_)
 
    io.out.stallALU0 := stallALU0_
    io.out.stallALU1 := stallALU1_
    io.out.stallMEM  := stallMEM_
    io.out.stallMUL  := stallMUL_

    io.out.MUX_control.MUX_ALU_0L := MUX_ALU_0L_
    io.out.MUX_control.MUX_ALU_0R := MUX_ALU_0R_
    io.out.MUX_control.MUX_ALU_1L := MUX_ALU_1L_
    io.out.MUX_control.MUX_ALU_1R := MUX_ALU_1R_
    io.out.MUX_control.MUX_MEM_L  := MUX_MEM_L_
    io.out.MUX_control.MUX_MEM_R  := MUX_MEM_R_
    io.out.MUX_control.MUX_MUL_L  := MUX_MUL_L_
    io.out.MUX_control.MUX_MUL_R  := MUX_MUL_R_

    io.out.muxStallALU0L := !muxStallALU0L_ && muxStallALU0R_
    io.out.muxStallALU0R := !muxStallALU0R_ && muxStallALU0L_
    io.out.muxStallALU1L := !muxStallALU1L_ && muxStallALU1R_
    io.out.muxStallALU1R := !muxStallALU1R_ && muxStallALU1L_
    // io.out.muxStallMemL  := stallMEM_ && (MUX_MEM_L_ =/= 4.U)
    // io.out.muxStallMemR  := stallMEM_ && (MUX_MEM_R_ =/= 4.U)
    io.out.muxStallMemL := stallMEM_ && !sourceOfRegs(srcMEMLreg_)(2) && !muxStallMemL_
    io.out.muxStallMemR := stallMEM_ && !sourceOfRegs(srcMEMRreg_)(2) && !muxStallMemR_
    // io.out.muxStallMemL  := false.B
    // io.out.muxStallMemR  := false.B
    io.out.muxStallMULL  := !muxStallMULL_ && muxStallMULR_
    io.out.muxStallMULR  := muxStallMULR_ && !muxStallMULL_
}

object BroadcastWakeupController extends App {
    ChiselStage.emitSystemVerilogFile(
      new BroadcastWakeupController(nPsyReg = 64),
      Array("--target-dir", "generated")
    )
}
