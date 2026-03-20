// Designed by Kyle. 2025-03-13 21:13
// Modified for Compact Queues. 2025-04-24
// Modified for Distributed Queues. 2025-05-12
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import constant._
import CONSTANTS._
import chisel3.util._
import CONSTANTS.CONFIG.PERF_STATS
import CONSTANTS.CONFIG.nPsyRegs
import CONSTANTS.CONFIG.debugSignal

class inst_in extends Bundle {
    val srcL             = Input(UInt(6.W))
    val valL             = Input(Bool())
    val srcR             = Input(UInt(6.W))
    val valR             = Input(Bool())
    val dest             = Input(UInt(6.W))
    val uiL              = Input(UInt(32.W))
    val uiR              = Input(UInt(32.W))
    val branch_inst      = Input(UInt(4.W))
    val FU               = Input(UInt(2.W)) // Use the ChiselEnum
    val ALU_OP           = Input(UInt(4.W))
    val reg_write        = Input(Bool())
    val mem_write        = Input(Bool())
    val mem_read         = Input(Bool())
    val mem_size         = Input(UInt(2.W))
    val PC               = Input(UInt(32.W))
    val PC_next          = Input(UInt(32.W))
    val PC_decode        = Input(UInt(32.W))
    val decode_PC_set    = Input(Bool())
    val offset_predicted = Input(UInt(2.W))
    val predicted_taken  = Input(Bool())    // 分支预测器的预测结果
}

class entry(nROBEntries: Int) extends Bundle {
    val PC               = UInt(32.W)
    val PC_next          = UInt(32.W)
    val PC_decode        = UInt(32.W)
    val offset_predicted = UInt(2.W)
    val decode_PC_set    = Bool()
    val predicted_taken  = Bool()    // 分支预测器的预测结果
    val ROB_index        = UInt(log2Ceil(nROBEntries).W)
    val branch_inst      = UInt(4.W)
    val uiL              = UInt(32.W)
    val uiR              = UInt(32.W)
    val srcL             = UInt(6.W)
    val valL             = Bool()    // Is srcL a register source (needs wakeup)?
    val srcR             = UInt(6.W)
    val valR             = Bool()    // Is srcR a register source (needs wakeup)?
    val dest             = UInt(6.W)
    val FU               = UInt(2.W) // Use the ChiselEnum
    val ALU_OP           = UInt(4.W)
    val reg_write        = Bool()
    val mem_write        = Bool()
    val mem_read         = Bool()    // Added mem_read here
    val mem_size         = UInt(2.W) // Added mem_size here
    val valid            = Bool()    // Is this entry valid?
}

class ALU_out(nROBEntries: Int, nPsyReg: Int) extends Bundle {
    val PC               = Output(UInt(32.W))
    val PC_next          = Output(UInt(32.W))
    val PC_decode        = Output(UInt(32.W))
    val decode_PC_set    = Output(Bool())
    val offset_predicted = Output(UInt(2.W))
    val predicted_taken  = Output(Bool()) // 分支预测器的预测结果
    val uiL              = Output(UInt(32.W))
    val uiR              = Output(UInt(32.W))
    val srcL             = Output(UInt(log2Ceil(nPsyReg).W))
    val valL        = Output(Bool()) // Indicates if srcL is used (not immediate/zero)
    val srcR        = Output(UInt(log2Ceil(nPsyReg).W))
    val valR        = Output(Bool()) // Indicates if srcR is used (not immediate/zero)
    val dest        = Output(UInt(log2Ceil(nPsyReg).W))
    val reg_write   = Output(Bool())
    val branch_inst = Output(UInt(4.W))
    val ALU_OP      = Output(UInt(4.W))
    val ROB_index   = Output(UInt(log2Ceil(nROBEntries).W))
}

class MEM_out(nROBEntries: Int, nPsyReg: Int) extends Bundle {
    val ROB_index = Output(UInt(log2Ceil(nROBEntries).W))
    val srcL      = Output(UInt(log2Ceil(nPsyReg).W)) // Base Address Reg
    val valL      = Output(Bool())
    val srcR = Output(
      UInt(log2Ceil(nPsyReg).W)
    ) // Data Reg (for store) / Not used for Load value
    val valR     = Output(Bool())
    val dest     = Output(UInt(log2Ceil(nPsyReg).W)) // Dest Reg (for load)
    val ui       = Output(UInt(32.W))                // Immediate Offset (using uiL field)
    val mem_size = Output(UInt(2.W))
    val reg_write = Output(Bool()) // For Load
    val mem_write = Output(Bool())
    val mem_read  = Output(Bool())
}

class MUL_out(nROBEntries: Int, nPsyReg: Int) extends Bundle {
    val ROB_index = Output(UInt(log2Ceil(nROBEntries).W))
    val srcL      = Output(UInt(log2Ceil(nPsyReg).W))
    val valL      = Output(Bool()) // Added valL/valR for consistency if needed
    val srcR      = Output(UInt(log2Ceil(nPsyReg).W))
    val valR      = Output(Bool())
    val dest      = Output(UInt(log2Ceil(nPsyReg).W))
    val reg_write = Output(Bool())
}

class regsInfo(nPsyReg: Int) extends Bundle {
    val srcL = Output(UInt(log2Ceil(nPsyReg).W))
    val valL = Output(Bool())
    val srcR = Output(UInt(log2Ceil(nPsyReg).W))
    val valR = Output(Bool())
    val dest = Output(UInt(log2Ceil(nPsyReg).W))
}

class CompactQueue[T <: Bundle](
  nEntries: Int,
  nROBEntries: Int,
  nPsyReg: Int,
  issueType: T,
  inOrder: Boolean,
  doIssue: (T, entry) => Unit
) extends Module {
    val in = IO(new Bundle {
        val instValid        = Vec(4, Input(Bool()))
        val inst             = Vec(4, new inst_in())
        val last_index       = Input(UInt(log2Ceil(nROBEntries).W)) // For recovery mode
        val ROB_head_pointer = Input(UInt(log2Ceil(nROBEntries).W)) // For recovery
        val ROB_tail_pointer = Input(UInt(log2Ceil(nROBEntries).W)) // For recovery
        val indexes   = Vec(4, Input(UInt(log2Ceil(nROBEntries).W))) // 指令在ROB中的索引
        val busyRegs  = Input(Vec(nPsyReg, Bool()))
        val recover   = Input(Bool())
        val clear     = Input(Bool())
        val execStall = Input(Bool())
        val pause     = Input(Bool())
    })
    val out_flags = IO(new Bundle {
        val numEntries = Output(UInt(log2Ceil(nEntries + 1).W))
        val issueValid = Output(Bool())
    })
    val out_issued = IO(Output(issueType))
    val regsInfo   = IO(Output(new regsInfo(nPsyReg)))
    val validWire  = IO(Output(Bool()))

    val regIssued = RegInit(0.U.asTypeOf((out_issued)))
    val writePtr  = RegInit(0.U(log2Ceil(nEntries).W))
    val queue = RegInit(VecInit(Seq.fill(nEntries)(0.U.asTypeOf(new entry(nROBEntries)))))

    // val numValidEntries = Wire(UInt(log2Ceil(nEntries + 1).W))
    val numValidEntries = RegInit(0.U(log2Ceil(nEntries + 1).W))

    val regIssueValid = RegInit(false.B)

    val nextQueue           = Wire(Vec(nEntries, new entry(nROBEntries)))
    val nextNumValidEntries = Wire(UInt(log2Ceil(nEntries + 1).W))

    val recoverMode = RegInit(false.B)
    val last_index  = RegInit(0.U(log2Ceil(nROBEntries).W))

    val regROBTailPointer = RegInit(0.U(log2Ceil(nROBEntries).W))

    when(in.clear) {
        recoverMode := false.B
        last_index  := 0.U
    }.otherwise {
        recoverMode := in.recover || recoverMode
    }

    when(in.recover && !recoverMode) {
        last_index        := in.last_index
        regROBTailPointer := in.ROB_tail_pointer
    }

    // Initialize variables
    for (i <- 0 until nEntries) {
        nextQueue(i)       := 0.U.asTypeOf(new entry(nROBEntries))
        nextQueue(i).valid := false.B
    }
    nextNumValidEntries := 0.U

    /* ----- Wakeup Logic ----- */
    val currentRdyL = Wire(Vec(nEntries, Bool()))
    val currentRdyR = Wire(Vec(nEntries, Bool()))

    for (i <- 0 until nEntries) {
        val entry        = queue(i)
        val isValidEntry = entry.valid

        val rdyL_if_literal        = !entry.valL
        val rdyL_if_x0             = entry.valL && (entry.srcL === 0.U)
        val rdyL_was_already_ready = !in.busyRegs(entry.srcL)
        currentRdyL(i) := isValidEntry &&
            (rdyL_if_literal || rdyL_if_x0 || rdyL_was_already_ready)

        val rdyR_if_literal        = !entry.valR
        val rdyR_if_x0             = entry.valR && (entry.srcR === 0.U)
        val rdyR_was_already_ready = !in.busyRegs(entry.srcR)

        currentRdyR(i) := isValidEntry &&
            (rdyR_if_literal || rdyR_if_x0 || rdyR_was_already_ready)
    }

    /* ----- Selection Logic ----- */
    val willBeIssued = WireInit(VecInit(Seq.fill(nEntries)(false.B)))

    // Move selValid and selIndex outside the if-else so they are always in scope
    val selValid = Wire(Bool())
    val selIndex = Wire(UInt(log2Ceil(nEntries).W))

    debugSignal(selValid)
    debugSignal(selIndex)

    when(in.execStall) {
        selValid := false.B
        selIndex := 0.U
    }.otherwise {
        if (inOrder) {
            selValid := queue(0).valid && currentRdyL(0) && currentRdyR(0)
            selIndex := 0.U
        } else {
            val candidates = Wire(Vec(nEntries, Bool()))
            for (i <- 0 until nEntries) {
                val entry = queue(i)
                candidates(i) := entry.valid && currentRdyL(i) && currentRdyR(i)
            }
            selValid := candidates.asUInt.orR
            selIndex := PriorityEncoder(candidates)
        }
    }

    when(in.execStall && !in.clear) {
        regIssued     := regIssued
        regIssueValid := regIssueValid
        regsInfo.srcL := queue(selIndex).srcL
        regsInfo.valL := queue(selIndex).valL
        regsInfo.srcR := queue(selIndex).srcR
        regsInfo.valR := queue(selIndex).valR
        regsInfo.dest := queue(selIndex).dest
        validWire := true.B
    }.elsewhen(selValid && !in.clear) {
        willBeIssued(selIndex) := true.B
        val selectedEntry = queue(selIndex)
        doIssue(regIssued, selectedEntry)
        regIssueValid := true.B
        regsInfo.srcL := selectedEntry.srcL
        regsInfo.valL := selectedEntry.valL
        regsInfo.srcR := selectedEntry.srcR
        regsInfo.valR := selectedEntry.valR
        regsInfo.dest := selectedEntry.dest
        validWire := true.B
    }.otherwise {
        regIssued     := 0.U.asTypeOf(out_issued)
        regIssueValid := false.B
        regsInfo      := 0.U.asTypeOf(regsInfo)
        validWire     := false.B
    }

    /* ----- Compaction Logic ----- */
    val remainValid = Wire(Vec(nEntries, Bool()))

    debugSignal(remainValid)
    debugSignal(willBeIssued)

    for (i <- 0 until nEntries) {
        remainValid(i) := queue(i).valid && !willBeIssued(i)
    }

    val newPositions = Wire(Vec(nEntries, UInt(log2Ceil(nEntries).W)))
    newPositions(0) := 0.U
    for (i <- 1 until nEntries) {
        newPositions(i) := Mux(
          remainValid(i - 1),
          newPositions(i - 1) + 1.U,
          newPositions(i - 1)
        )
    }

    debugSignal(newPositions)

    for (i <- 0 until nEntries) {
        when(!recoverMode && remainValid(i)) {
            nextQueue(newPositions(i)) := queue(i)
        }
    }

    /* ----- Insertion Logic ----- */
    val numAvailableSlots = nEntries.U - writePtr
    val numIncoming       = PopCount(in.instValid)
    val numToInsert = Mux(numIncoming > numAvailableSlots, numAvailableSlots, numIncoming)

    val insertBaseIdx = writePtr - selValid.asUInt
    debugSignal(insertBaseIdx)
    val insertOffset = Wire(Vec(4, UInt(log2Ceil(nEntries).W)))
    insertOffset(0) := 0.U
    for (i <- 1 until 4) {
        insertOffset(i) := insertOffset(i - 1) + in.instValid(i - 1)
    }

    // when(!recoverMode && !in.clear && !reset.asBool && !in.recover) {
    when(!recoverMode && !in.clear && !reset.asBool && !in.pause) {
        for (i <- 0 until 4) {
            when(in.instValid(i) && insertOffset(i) < numToInsert) {
                val insertIdx =
                    (insertBaseIdx + insertOffset(i))(log2Ceil(nEntries) - 1, 0)
                val currentInst = in.inst(i)
                nextQueue(insertIdx).PC               := currentInst.PC
                nextQueue(insertIdx).PC_next          := currentInst.PC_next
                nextQueue(insertIdx).PC_decode        := currentInst.PC_decode
                nextQueue(insertIdx).decode_PC_set    := currentInst.decode_PC_set
                nextQueue(insertIdx).offset_predicted := currentInst.offset_predicted
                nextQueue(insertIdx).predicted_taken  := currentInst.predicted_taken
                nextQueue(insertIdx).ROB_index        := in.indexes(i)
                nextQueue(insertIdx).branch_inst      := currentInst.branch_inst
                nextQueue(insertIdx).uiL              := currentInst.uiL
                nextQueue(insertIdx).uiR              := currentInst.uiR
                nextQueue(insertIdx).srcL             := currentInst.srcL
                nextQueue(insertIdx).valL             := currentInst.valL
                nextQueue(insertIdx).srcR             := currentInst.srcR
                nextQueue(insertIdx).valR             := currentInst.valR
                nextQueue(insertIdx).dest             := currentInst.dest
                nextQueue(insertIdx).FU               := currentInst.FU
                nextQueue(insertIdx).ALU_OP           := currentInst.ALU_OP
                nextQueue(insertIdx).reg_write        := currentInst.reg_write
                nextQueue(insertIdx).mem_write        := currentInst.mem_write
                nextQueue(insertIdx).mem_read         := currentInst.mem_read
                nextQueue(insertIdx).mem_size         := currentInst.mem_size
                nextQueue(insertIdx).valid            := true.B
            }
        }
    }.otherwise {
        nextNumValidEntries := 0.U
        when(recoverMode && !in.clear) {
            // Recovery: Selectively copy entries that are NOT invalidated
            val head = in.ROB_head_pointer
            val tail = regROBTailPointer
            val last = last_index

            // Determine which entries should remain valid after recovery
            val shouldKeep = Wire(Vec(nEntries, Bool()))
            for (i <- 0 until nEntries) {
                val entry            = queue(i)
                val entryRemainValid = remainValid(i)
                val idx              = entry.ROB_index

                // Determine if the entry should be invalidated based on ROB pointers and last_index
                // Invalidate if the index is between last_index (inclusive) and tail (exclusive),
                // handling wrap-around.
                val shouldInvalidate = Mux(
                  tail >= last, // Normal case (no wrap-around)
                  (idx >= last) && (idx < tail),
                  // Wrap-around case
                  (idx >= last) || (idx < tail) // Invalidate if >= last OR < tail
                )
                debugSignal(shouldInvalidate)
                // Keep the entry only if it's currently valid AND should NOT be invalidated by recovery
                shouldKeep(i) := entryRemainValid && !shouldInvalidate
            }

            // Calculate new positions for kept entries using prefix sum
            val keepPositions = Wire(Vec(nEntries, UInt(log2Ceil(nEntries).W)))
            keepPositions(0) := 0.U
            for (i <- 1 until nEntries) {
                keepPositions(i) := Mux(
                  shouldKeep(i - 1),
                  keepPositions(i - 1) + 1.U,
                  keepPositions(i - 1)
                )
            }

            // Copy entries to their new positions
            for (i <- 0 until nEntries) {
                when(shouldKeep(i)) {
                    nextQueue(keepPositions(i)) := queue(i)
                }
            }

            // Set the number of valid entries after recovery compaction
            nextNumValidEntries := PopCount(shouldKeep)
        }.otherwise {
            // clear
            for (i <- 0 until nEntries) {
                nextQueue(i).valid := false.B
            }
        }
    }

    /* ----- All tasks done. Update. ----- */
    queue := nextQueue
    numValidEntries := Mux(
      in.clear,
      0.U,
      PopCount(nextQueue.map((entry: entry) => entry.valid))
    )
    writePtr := Mux(in.clear, 0.U, writePtr + numToInsert - selValid.asUInt)

    out_issued           := regIssued
    out_flags.numEntries := numValidEntries
    out_flags.issueValid := regIssueValid
}

class IssueQueueDistributed(
  aluQueueSize: Int,
  memQueueSize: Int,
  mulQueueSize: Int,
  nROBEntries: Int,
  nPsyReg: Int
) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val instValid        = Vec(4, Input(Bool()))                // 指令有效
            val last_index       = Input(UInt(log2Ceil(nROBEntries).W)) // 供恢复模式用
            val ROB_head_pointer = Input(UInt(log2Ceil(nROBEntries).W)) // 供恢复模式用
            val ROB_tail_pointer = Input(UInt(log2Ceil(nROBEntries).W)) // 供恢复模式用
            val inst = Vec(
              4,
              new Bundle {
                  val srcL             = Input(UInt(log2Ceil(nPsyReg).W))
                  val valL             = Input(Bool())
                  val srcR             = Input(UInt(log2Ceil(nPsyReg).W))
                  val valR             = Input(Bool())
                  val dest             = Input(UInt(log2Ceil(nPsyReg).W))
                  val uiL              = Input(UInt(32.W))
                  val uiR              = Input(UInt(32.W))
                  val branch_inst      = Input(UInt(4.W))
                  val FU               = Input(UInt(2.W)) // Use the ChiselEnum
                  val ALU_OP           = Input(UInt(4.W))
                  val reg_write        = Input(Bool())
                  val mem_write        = Input(Bool())
                  val mem_read         = Input(Bool())
                  val mem_size         = Input(UInt(2.W))
                  val PC               = Input(UInt(32.W))
                  val PC_next          = Input(UInt(32.W))
                  val PC_decode        = Input(UInt(32.W))
                  val decode_PC_set    = Input(Bool())
                  val offset_predicted = Input(UInt(2.W)) // 分支预测偏移
                  val predicted_taken  = Input(Bool())    // 分支预测器的预测结果
              }
            )
            val indexes      = Vec(4, Input(UInt(log2Ceil(nROBEntries).W))) // 指令在ROB中的索引
            val commited     = Vec(4, Input(UInt(log2Ceil(nPsyReg).W)))
            val commitEnable = Vec(4, Input(Bool()))
            val execStall = Vec(4, Input(Bool())) // Stall from execution
            val pause     = Input(Bool())         // Pause signal
            val recover   = Input(Bool())
            val clear     = Input(Bool())
        }
        val out = new Bundle {
            val queueFull = Output(Bool()) // Can accept <= 4 instructions?
            val issueValid =
                Output(Vec(4, Bool())) // [0]=ALU0, [1]=ALU1, [2]=MEM, [3]=MUL

            val issued0   = Output(new ALU_out(nROBEntries, nPsyReg))
            val issued1   = Output(new ALU_out(nROBEntries, nPsyReg))
            val issued2   = Output(new MEM_out(nROBEntries, nPsyReg))
            val issued3   = Output(new MUL_out(nROBEntries, nPsyReg))
            val regsInfo0 = Output(new regsInfo(nPsyReg))
            val regsInfo1 = Output(new regsInfo(nPsyReg))
            val regsInfo2 = Output(new regsInfo(nPsyReg))
            val regsInfo3 = Output(new regsInfo(nPsyReg))

            val validWire = Output(Vec(4, Bool()))
        }
    })

    val ALU0ValidEntries = Wire(UInt(log2Ceil(aluQueueSize + 1).W))
    val ALU1ValidEntries = Wire(UInt(log2Ceil(aluQueueSize + 1).W))
    val MEMValidEntries  = Wire(UInt(log2Ceil(memQueueSize + 1).W))
    val MULValidEntries  = Wire(UInt(log2Ceil(mulQueueSize + 1).W))

    /* ----- Instantiate queues with coordinate issueLogic ----- */
    val alu0IssueLogic = (issued: ALU_out, selectedEntry: entry) => {
        issued.PC               := selectedEntry.PC
        issued.PC_next          := selectedEntry.PC_next
        issued.PC_decode        := selectedEntry.PC_decode
        issued.decode_PC_set    := selectedEntry.decode_PC_set
        issued.offset_predicted := selectedEntry.offset_predicted
        issued.predicted_taken  := selectedEntry.predicted_taken // 分支预测器的预测结果
        issued.uiL              := selectedEntry.uiL
        issued.uiR              := selectedEntry.uiR
        issued.srcL             := selectedEntry.srcL
        issued.valL             := selectedEntry.valL
        issued.srcR             := selectedEntry.srcR
        issued.valR             := selectedEntry.valR
        issued.dest             := selectedEntry.dest
        issued.reg_write        := selectedEntry.reg_write
        issued.branch_inst      := selectedEntry.branch_inst
        issued.ALU_OP           := selectedEntry.ALU_OP
        issued.ROB_index        := selectedEntry.ROB_index
    }

    val alu0Queue = Module(
      new CompactQueue(
        aluQueueSize,
        nROBEntries,
        nPsyReg,
        new ALU_out(nROBEntries, nPsyReg),
        inOrder = false,
        alu0IssueLogic
      )
    )
    val alu1Queue = Module(
      new CompactQueue(
        aluQueueSize,
        nROBEntries,
        nPsyReg,
        new ALU_out(nROBEntries, nPsyReg),
        inOrder = false,
        alu0IssueLogic
      )
    ) // Assuming same logic for alu1

    val memIssueLogic = (issued: MEM_out, selectedEntry: entry) => {
        issued.ROB_index := selectedEntry.ROB_index
        issued.srcL      := selectedEntry.srcL
        issued.valL      := selectedEntry.valL
        issued.srcR      := selectedEntry.srcR      // Data for store
        issued.valR      := selectedEntry.valR
        issued.dest      := selectedEntry.dest      // Dest for load
        issued.ui        := selectedEntry.uiL       // Offset from uiL
        issued.mem_size  := selectedEntry.mem_size
        issued.reg_write := selectedEntry.reg_write // Load writes reg
        issued.mem_write := selectedEntry.mem_write // Store/Load flag
        issued.mem_read  := selectedEntry.mem_read  // Added mem_read
    }
    val memQueue = Module(
      new CompactQueue(
        memQueueSize,
        nROBEntries,
        nPsyReg,
        new MEM_out(nROBEntries, nPsyReg),
        inOrder = true,
        memIssueLogic
      )
    )

    val mulIssueLogic = (issued: MUL_out, selectedEntry: entry) => {
        issued.ROB_index := selectedEntry.ROB_index
        issued.srcL      := selectedEntry.srcL
        issued.valL      := selectedEntry.valL
        issued.srcR      := selectedEntry.srcR
        issued.valR      := selectedEntry.valR
        issued.dest      := selectedEntry.dest
        issued.reg_write := selectedEntry.reg_write
    }
    val mulQueue = Module(
      new CompactQueue(
        mulQueueSize,
        nROBEntries,
        nPsyReg,
        new MUL_out(nROBEntries, nPsyReg),
        inOrder = false,
        mulIssueLogic
      )
    )

    ALU0ValidEntries := alu0Queue.out_flags.numEntries
    ALU1ValidEntries := alu1Queue.out_flags.numEntries
    MEMValidEntries  := memQueue.out_flags.numEntries
    MULValidEntries  := mulQueue.out_flags.numEntries

    debugSignal(ALU0ValidEntries)
    debugSignal(ALU1ValidEntries)
    debugSignal(MEMValidEntries)
    debugSignal(MULValidEntries)

    val busyRegs = RegInit(VecInit(Seq.fill(nPsyReg)(false.B)))
    // val busyRegsC = RegInit(VecInit(Seq.fill(nPsyReg)(false.B)))

    val regWakeUpRegMUL = ShiftRegister(
      io.out.regsInfo3.dest,
      EXECUTION_CYCLE.MUL - 1,
      0.U,
      !io.in.execStall(3)
    )

    when(!io.in.execStall(0)) {
        busyRegs(io.out.regsInfo0.dest) := false.B
    }

    when(!io.in.execStall(1)) {
        busyRegs(io.out.regsInfo1.dest) := false.B
    }

    when(!io.in.execStall(2)) {
        busyRegs(io.out.regsInfo2.dest) := false.B
    }

    when(!io.in.execStall(3)) {
        busyRegs(regWakeUpRegMUL) := false.B
    }

    /* ----- Connect inputs to queues ----- */
    val isALU =
        io.in.instValid.zip(io.in.inst).map { case (v, i) => v && (i.FU === FU_TYPE.ALU) }
    val isMEM =
        io.in.instValid.zip(io.in.inst).map { case (v, i) => v && (i.FU === FU_TYPE.MEM) }
    val isMUL =
        io.in.instValid.zip(io.in.inst).map { case (v, i) => v && (i.FU === FU_TYPE.MUL) }

    // 2. ALU型指令平均分配到两个ALU队列
    // 记录分配到ALU0和ALU1的指令索引
    val aluIndices = Wire(Vec(4, UInt(2.W)))

    val loadCounterWidth = log2Ceil(aluQueueSize + 5)

    val alu0LoadForDispatch = Wire(Vec(5, UInt(loadCounterWidth.W)))
    val alu1LoadForDispatch = Wire(Vec(5, UInt(loadCounterWidth.W)))

    alu0LoadForDispatch(0) := ALU0ValidEntries
    alu1LoadForDispatch(0) := ALU1ValidEntries // Corrected: Use alu1Queue's count

    for (i <- 0 until 4) {
        when(isALU(i)) {
            when(alu0LoadForDispatch(i) <= alu1LoadForDispatch(i)) {
                aluIndices(i)              := 0.U // Assign to ALU0
                alu0LoadForDispatch(i + 1) := alu0LoadForDispatch(i) + 1.U
                alu1LoadForDispatch(i + 1) := alu1LoadForDispatch(i)
            }.otherwise {
                aluIndices(i)              := 1.U // Assign to ALU1
                alu0LoadForDispatch(i + 1) := alu0LoadForDispatch(i)
                alu1LoadForDispatch(i + 1) := alu1LoadForDispatch(i) + 1.U
            }
        }.otherwise {
            aluIndices(i) := 3.U // Not an ALU instruction. This index won't match 0 or 1 for ALU queues.
            alu0LoadForDispatch(i + 1) := alu0LoadForDispatch(i)
            alu1LoadForDispatch(i + 1) := alu1LoadForDispatch(i)
        }
    }

    // 3. 生成各子队列的输入信号
    def fillQueueInputs[T <: Bundle](
      queue: CompactQueue[T],
      sel: Int => Bool,
      index: Int
    ): Unit = {
        for (i <- 0 until 4) {
            queue.in.instValid(i) := io.in.instValid(i) && sel(i)
            queue.in.inst(i)      := io.in.inst(i)
            queue.in.indexes(i)   := io.in.indexes(i)
        }
        queue.in.last_index       := io.in.last_index
        queue.in.ROB_head_pointer := io.in.ROB_head_pointer
        queue.in.ROB_tail_pointer := io.in.ROB_tail_pointer
        queue.in.recover          := io.in.recover
        queue.in.clear            := io.in.clear
        queue.in.busyRegs         := busyRegs
        queue.in.execStall        := io.in.execStall(index)
        queue.in.pause            := io.in.pause
    }

    val ALUInstsIn = Wire(UInt(3.W))
    val MEMInstsIn = Wire(UInt(3.W))
    val MULInstsIn = Wire(UInt(3.W))

    ALUInstsIn := PopCount(isALU.zip(io.in.instValid).map { case (alu, valid) =>
        alu && valid
    })
    MEMInstsIn := PopCount(isMEM.zip(io.in.instValid).map { case (mem, valid) =>
        mem && valid
    })
    MULInstsIn := PopCount(isMUL.zip(io.in.instValid).map { case (mul, valid) =>
        mul && valid
    })

    debugSignal(ALUInstsIn)
    debugSignal(MEMInstsIn)
    debugSignal(MULInstsIn)

    val queueFull =
        (ALU0ValidEntries +& ALU1ValidEntries +& ALUInstsIn > (2 * aluQueueSize).U) ||
            (MEMValidEntries +& MEMInstsIn > memQueueSize.U) ||
            (MULValidEntries +& MULInstsIn > mulQueueSize.U)

    when(!queueFull) {
        fillQueueInputs(alu0Queue, i => isALU(i) && aluIndices(i) === 0.U, 0)
        fillQueueInputs(alu1Queue, i => isALU(i) && aluIndices(i) === 1.U, 1)
        fillQueueInputs(memQueue, i => isMEM(i), 2)
        fillQueueInputs(mulQueue, i => isMUL(i), 3)
    }.otherwise {
        fillQueueInputs(alu0Queue, _ => false.B, 0)
        fillQueueInputs(alu1Queue, _ => false.B, 1)
        fillQueueInputs(memQueue, _ => false.B, 2)
        fillQueueInputs(mulQueue, _ => false.B, 3)
    }

    for (i <- 0 until 4) {
        when(io.in.instValid(i) && io.in.inst(i).dest =/= 0.U) {
            busyRegs(io.in.inst(i).dest) := true.B
        }
    }

    when(io.in.clear) {
        for (i <- 0 until nPsyReg) {
            busyRegs(i) := false.B
        }
    }

    io.out.issued0       := alu0Queue.out_issued
    io.out.issueValid(0) := alu0Queue.out_flags.issueValid
    io.out.regsInfo0     := alu0Queue.regsInfo

    io.out.issued1       := alu1Queue.out_issued
    io.out.issueValid(1) := alu1Queue.out_flags.issueValid
    io.out.regsInfo1     := alu1Queue.regsInfo

    io.out.issued2       := memQueue.out_issued
    io.out.issueValid(2) := memQueue.out_flags.issueValid
    io.out.regsInfo2     := memQueue.regsInfo

    io.out.issued3       := mulQueue.out_issued
    io.out.issueValid(3) := mulQueue.out_flags.issueValid
    io.out.regsInfo3     := mulQueue.regsInfo

    io.out.validWire := VecInit(alu0Queue.validWire,
                            alu1Queue.validWire,
                            memQueue.validWire,
                            mulQueue.validWire)

    io.out.queueFull := queueFull

    if (PERF_STATS) {
        val alu0Insts = RegInit(0.U(32.W))
        val alu1Insts = RegInit(0.U(32.W))
        val memInsts  = RegInit(0.U(32.W))
        val mulInsts  = RegInit(0.U(32.W))

        when(io.out.issueValid(0) && !io.in.execStall(0)) {
            alu0Insts := alu0Insts + 1.U
        }
        when(io.out.issueValid(1) && !io.in.execStall(1)) {
            alu1Insts := alu1Insts + 1.U
        }
        when(io.out.issueValid(2) && !io.in.execStall(2)) {
            memInsts := memInsts + 1.U
        }
        when(io.out.issueValid(3) && !io.in.execStall(3)) {
            mulInsts := mulInsts + 1.U
        }
        debugSignal(alu0Insts)
        debugSignal(alu1Insts)
        debugSignal(memInsts)
        debugSignal(mulInsts)
    }
}

object IssueQueueDistributed extends App {
    ChiselStage.emitSystemVerilogFile(
      new IssueQueueDistributed(16, 8, 8, 32, 64),
      Array("--target-dir", "generated")
    )
}
