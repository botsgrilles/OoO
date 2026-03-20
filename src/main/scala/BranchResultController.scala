// Designed by Kyle. 2025-03-29 19:51
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import CONSTANTS.BRANCH_INST
import CONSTANTS.BRANCH_TYPE
import CONSTANTS.CONFIG._

/*
    Branch Result Controller 实际分支结果判断与控制

    TODO:
    - 修改之前流水线阶段的数据流，使之包含分支预测的方向和结果
    - 修改分支预测器的使能逻辑
 */
class BranchResultController(nROBEntries: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val valid0            = Input(Bool())
            val valid1            = Input(Bool())
            val PC0               = Input(UInt(32.W))
            val PC1               = Input(UInt(32.W))
            val PC_predicted0     = Input(UInt(32.W))
            val PC_predicted1     = Input(UInt(32.W))
            val PC_decode0        = Input(UInt(32.W))
            val PC_decode1        = Input(UInt(32.W))
            val decode_PC_set0    = Input(Bool())
            val decode_PC_set1    = Input(Bool())
            val offset_predicted0 = Input(UInt(2.W))  // 分支预测偏移
            val offset_predicted1 = Input(UInt(2.W))  // 分支预测偏移
            val predicted_taken0  = Input(Bool())     // 分支预测器的预测结果
            val predicted_taken1  = Input(Bool())     // 分支预测器的预测结果
            val regRead0          = Input(UInt(32.W))
            val regRead1          = Input(UInt(32.W))
            val ZF0               = Input(Bool())
            val SF0               = Input(Bool())
            val ZF1               = Input(Bool())
            val SF1               = Input(Bool())
            val F0                = Input(UInt(32.W)) // ALU计算结果
            val F1                = Input(UInt(32.W)) // ALU计算结果
            val uiL0              = Input(UInt(32.W))
            val uiL1              = Input(UInt(32.W))
            val uiR0              = Input(UInt(32.W))
            val uiR1              = Input(UInt(32.W))
            val destReg0          = Input(UInt(log2Up(nPsyRegs).W))
            val destReg1          = Input(UInt(log2Up(nPsyRegs).W))
            val ROBIndex0         = Input(UInt(log2Up(nROBEntries).W))
            val ROBIndex1         = Input(UInt(log2Up(nROBEntries).W))

            val ROBHead = Input(UInt(log2Up(nROBEntries).W))
            val ROBTail = Input(UInt(log2Up(nROBEntries).W))

            val branchInst0 = Input(UInt(4.W))
            val branchInst1 = Input(UInt(4.W))

            val RemoveMask = Input(Bool())
        }

        val out = new Bundle {
            /* To Both */
            /* To BranchArbiter */
            val mispredict  = Output(Bool())
            val lastValidID = Output(UInt(log2Up(nROBEntries).W))
            val target      = Output(UInt(32.W))
            /* To ROB */
            val PC_where_branched = Output(Vec(2, UInt(32.W)))
            val valid             = Output(Vec(2, Bool()))
            val direction_true    = Output(Vec(2, Bool()))
            val target_true       = Output(Vec(2, UInt(32.W)))
            val branch_type       = Output(Vec(2, UInt(2.W)))
        }
    })

    def isOlder(idxA: UInt, idxB: UInt, head: UInt): Bool = {
        val diffA = idxA - head
        val diffB = idxB - head
        diffA < diffB
    }

    val mask           = RegInit(false.B)
    val regLastValidID = RegInit(0.U(log2Up(nROBEntries).W))
    val regLastAddr    = RegInit(0.U(32.W))
    val regROBHead     = RegInit(0.U(log2Up(nROBEntries).W))
    val misPredict     = Wire(Vec(2, Bool()))

    val regRead0 = RegNext(io.in.regRead0)
    val regRead1 = RegNext(io.in.regRead1)

    val PC_where_branched = RegInit(VecInit(Seq.fill(2)(0.U(32.W))))
    val reg_misPredict    = RegInit(false.B)
    val target            = RegInit(0.U(32.W))
    val valid             = RegInit(VecInit(Seq.fill(2)(false.B)))
    val direction_true    = RegInit(VecInit(Seq.fill(2)(false.B)))
    val target_true       = RegInit(VecInit(Seq.fill(2)(0.U(32.W))))
    val branch_type       = RegInit(VecInit(Seq.fill(2)(0.U(2.W))))

    /* Calculate Ground Truth */
    val trueTaken0 = MuxLookup(io.in.branchInst0, false.B)(
      Array(
        BRANCH_INST.NOT  -> false.B,
        BRANCH_INST.BNE  -> !io.in.ZF0,
        BRANCH_INST.BEQ  -> io.in.ZF0,
        BRANCH_INST.JIRL -> true.B,
        BRANCH_INST.B    -> true.B,
        BRANCH_INST.BL   -> true.B,
        BRANCH_INST.BLT  -> (io.in.F0 === 1.U),
        BRANCH_INST.BGE  -> (io.in.F0 === 0.U),
        BRANCH_INST.BLTU -> (io.in.F0 === 1.U),
        BRANCH_INST.BGEU -> (io.in.F0 === 0.U)
      )
    )
    val trueTaken1 = MuxLookup(io.in.branchInst1, false.B)(
      Array(
        BRANCH_INST.NOT  -> false.B,
        BRANCH_INST.BNE  -> !io.in.ZF1,
        BRANCH_INST.BEQ  -> io.in.ZF1,
        BRANCH_INST.JIRL -> true.B,
        BRANCH_INST.B    -> true.B,
        BRANCH_INST.BL   -> true.B,
        BRANCH_INST.BLT  -> (io.in.F1 === 1.U),
        BRANCH_INST.BGE  -> (io.in.F1 === 0.U),
        BRANCH_INST.BLTU -> (io.in.F1 === 1.U),
        BRANCH_INST.BGEU -> (io.in.F1 === 0.U)
      )
    )

    // 在译码阶段已经完成偏移量计算
    val PC_inc1 = io.in.PC1 + 4.U

    val trueAddr1forUpdate = MuxLookup(io.in.branchInst1, 0.U)(
      Array(
        BRANCH_INST.NOT  -> PC_inc1,
        BRANCH_INST.BNE  -> Mux(io.in.ZF1, PC_inc1, io.in.uiL1),
        BRANCH_INST.BEQ  -> Mux(io.in.ZF1, io.in.uiL1, PC_inc1),
        BRANCH_INST.JIRL -> (regRead1 + io.in.uiR1),
        BRANCH_INST.B    -> io.in.uiL1,
        BRANCH_INST.BL   -> io.in.uiL1,
        BRANCH_INST.BLT  -> Mux(trueTaken1, io.in.uiL1, PC_inc1),
        BRANCH_INST.BGE  -> Mux(trueTaken1, io.in.uiL1, PC_inc1),
        BRANCH_INST.BLTU -> Mux(trueTaken1, io.in.uiL1, PC_inc1),
        BRANCH_INST.BGEU -> Mux(trueTaken1, io.in.uiL1, PC_inc1)
      )
    )

    val PC_inc0 = io.in.PC0 + 4.U

    val trueAddr0forUpdate = MuxLookup(io.in.branchInst0, 0.U)(
      Array(
        BRANCH_INST.NOT  -> PC_inc0,
        BRANCH_INST.BNE  -> Mux(io.in.ZF0, PC_inc0, io.in.uiL0),
        BRANCH_INST.BEQ  -> Mux(io.in.ZF0, io.in.uiL0, PC_inc0),
        BRANCH_INST.JIRL -> (regRead0 + io.in.uiR0),
        BRANCH_INST.B    -> io.in.uiL0,
        BRANCH_INST.BL   -> io.in.uiL0,
        BRANCH_INST.BLT  -> Mux(trueTaken0, io.in.uiL0, PC_inc0),
        BRANCH_INST.BGE  -> Mux(trueTaken0, io.in.uiL0, PC_inc0),
        BRANCH_INST.BLTU -> Mux(trueTaken0, io.in.uiL0, PC_inc0),
        BRANCH_INST.BGEU -> Mux(trueTaken0, io.in.uiL0, PC_inc0)
      )
    )

    debugSignal(trueAddr0forUpdate)
    debugSignal(trueAddr1forUpdate)

    def branchInst2branchType(branchInst: UInt, destReg: UInt): UInt = {
        MuxLookup(branchInst, 0.U)(
          Array(
            BRANCH_INST.NOT -> 0.U,
            BRANCH_INST.BNE -> BRANCH_TYPE.CONDITION,
            BRANCH_INST.BEQ -> BRANCH_TYPE.CONDITION,
            BRANCH_INST.JIRL -> Mux(
              destReg === 0.U,
              BRANCH_TYPE.RETURN,
              BRANCH_TYPE.CALL
            ),
            BRANCH_INST.B   -> BRANCH_TYPE.DIRECT,
            BRANCH_INST.BL  -> BRANCH_TYPE.CALL,
            BRANCH_INST.BLT -> BRANCH_TYPE.CONDITION,
            BRANCH_INST.BGE -> BRANCH_TYPE.CONDITION
          )
        )
    }

    val isBranchInst0 = io.in.branchInst0 =/= BRANCH_INST.NOT
    val isBranchInst1 = io.in.branchInst1 =/= BRANCH_INST.NOT

    val isNotDirectBranch0 = io.in.branchInst0 =/= BRANCH_INST.B &&
        io.in.branchInst0 =/= BRANCH_INST.BL

    val isNotDirectBranch1 = io.in.branchInst1 =/= BRANCH_INST.B &&
        io.in.branchInst1 =/= BRANCH_INST.BL

    misPredict(0) := io.in.valid0 && isBranchInst0 && isNotDirectBranch0 && ((io.in.predicted_taken0 =/= trueTaken0) ||
        trueTaken0 && io.in.predicted_taken0 && io.in.PC_predicted0 =/= trueAddr0forUpdate)
    misPredict(1) := io.in.valid1 && isBranchInst1 && isNotDirectBranch1 && ((io.in.predicted_taken1 =/= trueTaken1) ||
        trueTaken1 && io.in.predicted_taken1 && io.in.PC_predicted1 =/= trueAddr1forUpdate)

    val misPredictWithMask = Wire(Vec(2, Bool()))
    misPredictWithMask(0) := Mux(
      mask,
      isOlder(io.in.ROBIndex0, regLastValidID, regROBHead),
      true.B
    ) && misPredict(0)
    misPredictWithMask(1) := Mux(
      mask,
      isOlder(io.in.ROBIndex1, regLastValidID, regROBHead),
      true.B
    ) && misPredict(1)

    PC_where_branched(0) := io.in.PC0
    PC_where_branched(1) := io.in.PC1

    io.out.PC_where_branched(0) := PC_where_branched(0)
    io.out.PC_where_branched(1) := PC_where_branched(1)

    /* To ROB */
    when(!mask) {
        valid(0) := io.in.valid0 && io.in.branchInst0 =/= BRANCH_INST.NOT
        valid(1) := io.in.valid1 && io.in.branchInst1 =/= BRANCH_INST.NOT
    }.otherwise {
        valid(0) := io.in.valid0 &&
            io.in.branchInst0 =/= BRANCH_INST.NOT &&
            isOlder(io.in.ROBIndex0, regLastValidID, io.in.ROBHead)
        valid(1) := io.in.valid1 &&
            io.in.branchInst1 =/= BRANCH_INST.NOT &&
            isOlder(io.in.ROBIndex1, regLastValidID, io.in.ROBHead)
    }
    io.out.valid(0) := valid(0)
    io.out.valid(1) := valid(1)

    direction_true(0)        := trueTaken0
    direction_true(1)        := trueTaken1
    io.out.direction_true(0) := direction_true(0)
    io.out.direction_true(1) := direction_true(1)

    target_true(0)        := trueAddr0forUpdate
    target_true(1)        := trueAddr1forUpdate
    io.out.target_true(0) := target_true(0)
    io.out.target_true(1) := target_true(1)

    branch_type(0)        := branchInst2branchType(io.in.branchInst0, io.in.destReg0)
    branch_type(1)        := branchInst2branchType(io.in.branchInst1, io.in.destReg1)
    io.out.branch_type(0) := branch_type(0)
    io.out.branch_type(1) := branch_type(1)

    reg_misPredict := misPredictWithMask(0) || misPredictWithMask(1)

    io.out.mispredict := reg_misPredict

    val lastValidIDWire = MuxCase(
      0.U, // default value
      Seq(
        (isBranchInst1 && misPredict(0) && !misPredict(1)) -> io.in.ROBIndex0,
        (isBranchInst0 && !misPredict(0) && misPredict(1)) -> io.in.ROBIndex1,
        (misPredict(0) && misPredict(1)) -> Mux(
          isOlder(io.in.ROBIndex0, io.in.ROBIndex1, io.in.ROBHead),
          io.in.ROBIndex0,
          io.in.ROBIndex1
        ),
        (misPredict(0) && !isBranchInst1) -> io.in.ROBIndex0,
        (misPredict(1) && !isBranchInst0) -> io.in.ROBIndex1
      )
    )
    debugSignal(lastValidIDWire)
    io.out.lastValidID := RegNext(lastValidIDWire)

    target := MuxCase(
      0.U,
      Seq(
        (misPredict(0) && !misPredict(1)) -> trueAddr0forUpdate,
        (misPredict(1) && !misPredict(0)) -> trueAddr1forUpdate,
        (misPredict(0) && misPredict(1)) -> Mux(
          isOlder(io.in.ROBIndex0, io.in.ROBIndex1, io.in.ROBHead),
          trueAddr0forUpdate,
          trueAddr1forUpdate
        )
      )
    )

    io.out.target := target

    val removeMask = ShiftRegister(io.in.RemoveMask, 6, false.B, true.B)
    debugSignal(removeMask)

    when(misPredictWithMask(0) || misPredictWithMask(1)) {
        mask           := true.B
        regLastAddr    := target
        regLastValidID := lastValidIDWire
        regROBHead     := io.in.ROBHead
    }.elsewhen(removeMask || reset.asBool) {
        mask := false.B
    }.otherwise {
        mask := mask
    }

    if (PERF_STATS) {
        // 统计分支预测正确率
        val branchPredictions  = RegInit(0.U(32.W))
        val correctPredictions = RegInit(0.U(32.W))

        branchPredictions := branchPredictions + isBranchInst0.asUInt + isBranchInst1.asUInt
        correctPredictions := correctPredictions + (isBranchInst0 && !misPredict(0)).asUInt + (isBranchInst1 && !misPredict(1)).asUInt

        debugSignal(branchPredictions)
        debugSignal(correctPredictions)
    }

    val offset0   = io.in.ROBIndex0 - io.in.ROBHead
    val offset1   = io.in.ROBIndex1 - io.in.ROBHead
    val offsetReg = regLastValidID - io.in.ROBHead
    debugSignal(offset0)
    debugSignal(offset1)
    debugSignal(offsetReg)
}

object BranchResultController extends App {
    ChiselStage.emitSystemVerilogFile(
      new BranchResultController(nROBEntries = 32),
      Array("--target-dir", "generated")
    )
}
