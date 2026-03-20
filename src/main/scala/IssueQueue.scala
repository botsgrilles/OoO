// Designed by Kyle. 2025-03-13 21:13
// Modified for Compact Queues. 2025-04-24
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import constant._
import CONSTANTS.FU_TYPE
import moduload.Priority
import chisel3.util.PriorityEncoder
import CONSTANTS.CONFIG.debugSignal

class IssueQueue(nIQEntries: Int, nROBEntries: Int) extends Module {
    // Output bundle for ALU units
    class ALU_out extends Bundle {
        val PC          = Output(UInt(32.W))
        val PC_next     = Output(UInt(32.W))
        val uiL         = Output(UInt(32.W))
        val uiR         = Output(UInt(32.W))
        val srcL        = Output(UInt(6.W))
        val valL        = Output(Bool()) // Indicates if srcL is used (not immediate/zero)
        val srcR        = Output(UInt(6.W))
        val valR        = Output(Bool()) // Indicates if srcR is used (not immediate/zero)
        val dest        = Output(UInt(6.W))
        val reg_write   = Output(Bool())
        val branch_inst = Output(UInt(4.W))
        val ALU_OP      = Output(UInt(4.W))
        val ROB_index   = Output(UInt(log2Ceil(nROBEntries).W))
    }

    // Output bundle for MEM unit
    class MEM_out extends Bundle {
        val ROB_index = Output(UInt(log2Ceil(nROBEntries).W))
        val srcL      = Output(UInt(6.W)) // Base Address Reg
        val valL      = Output(Bool())
        val srcR = Output(UInt(6.W))  // Data Reg (for store) / Not used for Load value
        val valR = Output(Bool())
        val dest = Output(UInt(6.W))  // Dest Reg (for load)
        val ui   = Output(UInt(32.W)) // Immediate Offset (using uiL field)
        val mem_size  = Output(UInt(2.W))
        val reg_write = Output(Bool()) // For Load
        val mem_write = Output(Bool()) // Store vs Load
    }

    // Output bundle for MUL unit
    class MUL_out extends Bundle {
        val ROB_index = Output(UInt(log2Ceil(nROBEntries).W))
        val srcL      = Output(UInt(6.W))
        val valL      = Output(Bool()) // Added valL/valR for consistency if needed
        val srcR      = Output(UInt(6.W))
        val valR      = Output(Bool())
        val dest      = Output(UInt(6.W))
        val reg_write = Output(Bool())
    }

    val io = IO(new Bundle {
        val in = new Bundle {
            val instValid        = Vec(4, Input(Bool()))                // 指令有效
            val last_index       = Input(UInt(log2Ceil(nROBEntries).W)) // 供恢复模式用
            val ROB_head_pointer = Input(UInt(log2Ceil(nROBEntries).W)) // 供恢复模式用
            val ROB_tail_pointer = Input(UInt(log2Ceil(nROBEntries).W)) // 供恢复模式用
            val inst = Vec(
              4,
              new Bundle {
                  val srcL        = Input(UInt(6.W))
                  val valL        = Input(Bool())
                  val srcR        = Input(UInt(6.W))
                  val valR        = Input(Bool())
                  val dest        = Input(UInt(6.W))
                  val uiL         = Input(UInt(32.W))
                  val uiR         = Input(UInt(32.W))
                  val branch_inst = Input(UInt(4.W))
                  val FU          = Input(UInt(2.W)) // Use the ChiselEnum
                  val ALU_OP      = Input(UInt(4.W))
                  val reg_write   = Input(Bool())
                  val mem_write   = Input(Bool())
                  val mem_size    = Input(UInt(2.W))
                  val PC          = Input(UInt(32.W))
                  val PC_next     = Input(UInt(32.W))
              }
            )
            val indexes = Vec(4, Input(UInt(log2Ceil(nROBEntries).W))) // 指令在ROB中的索引
            val wake_up_regs  = Vec(4, Input(UInt(6.W))) // Match src/dest width
            val wake_up_valid = Vec(4, Input(Bool()))
            val recover       = Input(Bool())
            val clear         = Input(Bool())
        }
        val out = new Bundle {
            val queueFull = Output(Bool()) // Can accept <= 4 instructions?
            val issueValid =
                Output(Vec(4, Bool())) // [0]=ALU0, [1]=ALU1, [2]=MEM, [3]=MUL

            val issued0 = Output(new ALU_out)
            val issued1 = Output(new ALU_out)
            val issued2 = Output(new MEM_out)
            val issued3 = Output(new MUL_out)
        }
    })

    // Define the structure of an entry in the issue queue
    class IssueQueueEntry(nROBEntries: Int) extends Bundle {
        val PC          = UInt(32.W)
        val PC_next     = UInt(32.W)
        val ROB_index   = UInt(log2Ceil(nROBEntries).W)
        val branch_inst = UInt(4.W)
        val uiL         = UInt(32.W)
        val uiR         = UInt(32.W)
        val srcL        = UInt(6.W)
        val valL        = Bool()    // Is srcL a register source (needs wakeup)?
        val rdyL        = Bool()    // Is srcL ready?
        val srcR        = UInt(6.W)
        val valR        = Bool()    // Is srcR a register source (needs wakeup)?
        val rdyR        = Bool()    // Is srcR ready?
        val dest        = UInt(6.W)
        val FU          = UInt(2.W) // Use the ChiselEnum
        val ALU_OP      = UInt(4.W)
        val reg_write   = Bool()
        val mem_write   = Bool()
        val mem_size    = UInt(2.W) // Added mem_size here
        val valid       = Bool()    // Is this entry valid?
    }

    val regissued0 = RegInit(0.U.asTypeOf(io.out.issued0))
    val regissued1 = RegInit(0.U.asTypeOf(io.out.issued1))
    val regissued2 = RegInit(0.U.asTypeOf(io.out.issued2))
    val regissued3 = RegInit(0.U.asTypeOf(io.out.issued3))
    val issueValid = RegInit(VecInit(Seq.fill(4)(false.B)))
    val writePtr = RegInit(
      0.U(log2Ceil(nIQEntries).W)
    ) // Pointer for writing to the issue queue
    val regNumIssued = RegInit(0.U(2.W)) // Number of instructions issued this cycle

    // The Issue Queue storage - uses Registers
    val issueQueue = RegInit(
      VecInit(Seq.fill(nIQEntries)(0.U.asTypeOf(new IssueQueueEntry(nROBEntries))))
    )

    // Counter for valid entries
    val numValidEntries = RegInit(0.U(log2Ceil(nIQEntries + 1).W))

    // --- Wires for Next State Calculation ---
    val nextIssueQueue = Wire(Vec(nIQEntries, new IssueQueueEntry(nROBEntries)))

    val nextNumValidEntries = Wire(UInt(log2Ceil(nIQEntries + 1).W))

    // Initialize next state computation - defaults to invalid
    for (i <- 0 until nIQEntries) {
        nextIssueQueue(i)       := 0.U.asTypeOf(new IssueQueueEntry(nROBEntries))
        nextIssueQueue(i).valid := false.B
    }

    nextNumValidEntries := 0.U // Default case (e.g., clear)

    // Calculate updated ready flags based on current state (issueQueue) and incoming wakeups
    // These currentRdyL/R are used for selection logic in the current cycle
    val currentRdyL = Wire(Vec(nIQEntries, Bool()))
    val currentRdyR = Wire(Vec(nIQEntries, Bool()))

    for (i <- 0 until nIQEntries) {
        val entry        = issueQueue(i)
        val isValidEntry = entry.valid

        // Calculate rdyL for current entry in issueQueue
        val rdyL_if_literal        = !entry.valL
        val rdyL_if_x0             = entry.valL && (entry.srcL === 0.U)
        val rdyL_was_already_ready = entry.rdyL
        val rdyL_woken_up_now = entry.valL && (0 until 4)
            .map { j =>
                io.in.wake_up_valid(j) && (entry.srcL === io.in.wake_up_regs(j))
            }
            .foldLeft(false.B)(_ || _)
        currentRdyL(
          i
        ) := isValidEntry && (rdyL_if_literal || rdyL_if_x0 || rdyL_was_already_ready || rdyL_woken_up_now)

        // Calculate rdyR for current entry in issueQueue
        val rdyR_if_literal        = !entry.valR
        val rdyR_if_x0             = entry.valR && (entry.srcR === 0.U)
        val rdyR_was_already_ready = entry.rdyR
        val rdyR_woken_up_now = entry.valR && (0 until 4)
            .map { j =>
                io.in.wake_up_valid(j) && (entry.srcR === io.in.wake_up_regs(j))
            }
            .foldLeft(false.B)(_ || _)
        currentRdyR(
          i
        ) := isValidEntry && (rdyR_if_literal || rdyR_if_x0 || rdyR_was_already_ready || rdyR_woken_up_now)
    }

    // --- Selection Logic ---
    // Identify ready candidates for each FU type among the *currently valid* entries
    val aluCandidates = Wire(Vec(nIQEntries, Bool()))
    val memCandidates = Wire(Vec(nIQEntries, Bool()))
    val mulCandidates = Wire(Vec(nIQEntries, Bool()))
    val willBeIssued  = Wire(Vec(nIQEntries, Bool()))
    // Tracks which entries are selected THIS cycle

    for (i <- 0 until nIQEntries) {
        // An entry is ready if it's valid and both its operands are ready
        val entry   = issueQueue(i)
        val isReady = entry.valid && currentRdyL(i) && currentRdyR(i)

        aluCandidates(i) := isReady && (entry.FU === FU_TYPE.ALU)
        memCandidates(i) := isReady && (entry.FU === FU_TYPE.MEM)
        mulCandidates(i) := isReady && (entry.FU === FU_TYPE.MUL)
        willBeIssued(i)  := false.B // Initialize to not issued
    }

    // Select oldest ready for ALU0 (lowest index)
    val selAlu0OH    = PriorityEncoderOH(aluCandidates)
    val selAlu0Valid = selAlu0OH.reduce(_ || _) // Check if any bit is set
    val selAlu0Idx   = OHToUInt(selAlu0OH)

    // Select oldest ready for ALU1 (lowest index *not* selected for ALU0)
    val aluCandidates1 = Wire(Vec(nIQEntries, Bool()))
    for (i <- 0 until nIQEntries) {
        aluCandidates1(i) := aluCandidates(i) && !selAlu0OH(
          i
        ) // Ready ALU and not the one already selected for ALU0
    }
    val selAlu1OH    = PriorityEncoderOH(aluCandidates1)
    val selAlu1Valid = selAlu1OH.reduce(_ || _)
    val selAlu1Idx   = OHToUInt(selAlu1OH)

    // Select oldest ready for MEM
    val selMemOH    = PriorityEncoderOH(memCandidates)
    val selMemValid = selMemOH.reduce(_ || _)
    val selMemIdx   = OHToUInt(selMemOH)

    // Select oldest ready for MUL
    val selMulOH    = PriorityEncoderOH(mulCandidates)
    val selMulValid = selMulOH.reduce(_ || _)
    val selMulIdx   = OHToUInt(selMulOH)

    // --- Assign Outputs and Mark for Compaction ---
    issueValid := VecInit(selAlu0Valid, selAlu1Valid, selMemValid, selMulValid)

    when(selAlu0Valid) {
        willBeIssued(selAlu0Idx) := true.B // Mark for removal during compaction
        val selectedEntry = issueQueue(selAlu0Idx)
        regissued0.PC          := selectedEntry.PC
        regissued0.PC_next     := selectedEntry.PC_next
        regissued0.uiL         := selectedEntry.uiL
        regissued0.uiR         := selectedEntry.uiR
        regissued0.srcL        := selectedEntry.srcL
        regissued0.valL        := selectedEntry.valL
        regissued0.srcR        := selectedEntry.srcR
        regissued0.valR        := selectedEntry.valR
        regissued0.dest        := selectedEntry.dest
        regissued0.reg_write   := selectedEntry.reg_write
        regissued0.branch_inst := selectedEntry.branch_inst
        regissued0.ALU_OP      := selectedEntry.ALU_OP
        regissued0.ROB_index   := selectedEntry.ROB_index
    }

    when(selAlu1Valid) {
        willBeIssued(selAlu1Idx) := true.B
        val selectedEntry = issueQueue(selAlu1Idx)
        regissued1.PC          := selectedEntry.PC
        regissued1.PC_next     := selectedEntry.PC_next
        regissued1.uiL         := selectedEntry.uiL
        regissued1.uiR         := selectedEntry.uiR
        regissued1.srcL        := selectedEntry.srcL
        regissued1.valL        := selectedEntry.valL
        regissued1.srcR        := selectedEntry.srcR
        regissued1.valR        := selectedEntry.valR
        regissued1.dest        := selectedEntry.dest
        regissued1.reg_write   := selectedEntry.reg_write
        regissued1.branch_inst := selectedEntry.branch_inst
        regissued1.ALU_OP      := selectedEntry.ALU_OP
        regissued1.ROB_index   := selectedEntry.ROB_index
    }

    when(selMemValid) {
        willBeIssued(selMemIdx) := true.B
        val selectedEntry = issueQueue(selMemIdx)
        regissued2.ROB_index := selectedEntry.ROB_index
        regissued2.srcL      := selectedEntry.srcL
        regissued2.valL      := selectedEntry.valL
        regissued2.srcR      := selectedEntry.srcR      // Data for store
        regissued2.valR      := selectedEntry.valR
        regissued2.dest      := selectedEntry.dest      // Dest for load
        regissued2.ui        := selectedEntry.uiL       // Offset from uiL
        regissued2.mem_size  := selectedEntry.mem_size
        regissued2.reg_write := selectedEntry.reg_write // Load writes reg
        regissued2.mem_write := selectedEntry.mem_write // Store/Load flag
    }

    when(selMulValid) {
        willBeIssued(selMulIdx) := true.B
        val selectedEntry = issueQueue(selMulIdx)
        regissued3.ROB_index := selectedEntry.ROB_index
        regissued3.srcL      := selectedEntry.srcL
        regissued3.valL      := selectedEntry.valL
        regissued3.srcR      := selectedEntry.srcR
        regissued3.valR      := selectedEntry.valR
        regissued3.dest      := selectedEntry.dest
        regissued3.reg_write := selectedEntry.reg_write
    }

    // --- Compaction Logic ---
    // Create the next state of the queue by shifting valid, non-issued entries
    val numIssued = PopCount(willBeIssued)

    // Create a mask of entries that will remain after issuing
    val remainValid = Wire(Vec(nIQEntries, Bool()))
    for (i <- 0 until nIQEntries) {
        remainValid(i) := issueQueue(i).valid && !willBeIssued(i)
    }

    // Create prefix sum to compute new positions
    val newPositions = Wire(Vec(nIQEntries, UInt(log2Ceil(nIQEntries).W)))
    newPositions(0) := 0.U
    for (i <- 1 until nIQEntries) {
        newPositions(i) := newPositions(i - 1) + remainValid(i - 1).asUInt
    }

    // Apply compaction
    for (i <- 0 until nIQEntries) {
        when(remainValid(i)) {
            // Copy entry to its new position
            nextIssueQueue(newPositions(i)) := issueQueue(i)
            // Update ready flags: currentRdyL/R already includes wakeups for this cycle
            nextIssueQueue(newPositions(i)).rdyL := currentRdyL(i)
            nextIssueQueue(newPositions(i)).rdyR := currentRdyR(i)
        }
    }

    // Calculate the new write pointer
    writePtr := PopCount(remainValid)

    // --- Insertion Logic ---
    // Calculate how many new instructions can be inserted
    val numAvailableSlots = nIQEntries.U - writePtr // Slots available after compaction
    val numIncoming       = PopCount(io.in.instValid)
    // Clamp insertion count to available slots
    val numToInsert = Mux(numIncoming > numAvailableSlots, numAvailableSlots, numIncoming)

    // Calculate base index for insertion
    val insertBaseIdx = writePtr - numIssued
    // Use the num_issued from the previous cycle

    // Keep track of how many incoming instructions have been placed so far
    val insertOffset = Wire(Vec(4, UInt(log2Ceil(nIQEntries).W)))
    insertOffset(0) := 0.U
    for (i <- 1 until 4) {
        insertOffset(i) := insertOffset(i - 1) + io.in.instValid(i - 1)
    }

    // Perform insertion into the *next* queue state if not recovering/clearing
    when(!io.in.recover && !io.in.clear && !reset.asBool) {
        for (i <- 0 until 4) {
            // Check if this instruction is valid AND fits within the number we decided to insert
            when(io.in.instValid(i) && insertOffset(i) < numToInsert) {
                val insertIdx = (insertBaseIdx + insertOffset(i))(
                  log2Ceil(nIQEntries) - 1,
                  0
                ) // Calculate the insertion index
                val currentInst = io.in.inst(i)

                // Copy data from input to the nextIssueQueue entry
                nextIssueQueue(insertIdx).PC          := currentInst.PC
                nextIssueQueue(insertIdx).PC_next     := currentInst.PC_next
                nextIssueQueue(insertIdx).ROB_index   := io.in.indexes(i)
                nextIssueQueue(insertIdx).branch_inst := currentInst.branch_inst
                nextIssueQueue(insertIdx).uiL         := currentInst.uiL
                nextIssueQueue(insertIdx).uiR         := currentInst.uiR
                nextIssueQueue(insertIdx).srcL        := currentInst.srcL
                nextIssueQueue(insertIdx).valL        := currentInst.valL
                nextIssueQueue(insertIdx).srcR        := currentInst.srcR
                nextIssueQueue(insertIdx).valR        := currentInst.valR
                nextIssueQueue(insertIdx).dest        := currentInst.dest
                nextIssueQueue(insertIdx).FU          := currentInst.FU
                nextIssueQueue(insertIdx).ALU_OP      := currentInst.ALU_OP
                nextIssueQueue(insertIdx).reg_write   := currentInst.reg_write
                nextIssueQueue(insertIdx).mem_write   := currentInst.mem_write
                nextIssueQueue(insertIdx).mem_size    := currentInst.mem_size
                nextIssueQueue(insertIdx).valid       := true.B

                // Calculate initial ready flags for the new entry
                // rdyL
                val new_rdyL_if_literal = !currentInst.valL
                val new_rdyL_if_x0      = currentInst.valL && (currentInst.srcL === 0.U)
                val new_rdyL_if_woken_up = currentInst.valL && (0 until 4)
                    .map { k =>
                        io.in.wake_up_valid(k) && (currentInst.srcL === io.in
                            .wake_up_regs(k))
                    }
                    .foldLeft(false.B)(_ || _)
                nextIssueQueue(
                  insertIdx
                ).rdyL := new_rdyL_if_literal || new_rdyL_if_x0 || new_rdyL_if_woken_up

                // rdyR
                val new_rdyR_if_literal = !currentInst.valR
                val new_rdyR_if_x0      = currentInst.valR && (currentInst.srcR === 0.U)
                val new_rdyR_if_woken_up = currentInst.valR && (0 until 4)
                    .map { k =>
                        io.in.wake_up_valid(k) && (currentInst.srcR === io.in
                            .wake_up_regs(k))
                    }
                    .foldLeft(false.B)(_ || _)
                nextIssueQueue(
                  insertIdx
                ).rdyR := new_rdyR_if_literal || new_rdyR_if_x0 || new_rdyR_if_woken_up
            }
        }
        // Calculate the final number of valid entries for the next cycle
        nextNumValidEntries := writePtr + numToInsert - numIssued
    }.otherwise { // Handle clear or recover
        nextNumValidEntries := 0.U // Already defaulted to 0, just making it explicit
        when(io.in.recover) {
            // Recovery: Selectively copy entries that are NOT invalidated
            val head           = io.in.ROB_head_pointer
            val tail           = io.in.ROB_tail_pointer
            val last           = io.in.last_index
            var nextValidCount = 0 // Use a Scala var for sequential logic generation

            for (i <- 0 until nIQEntries) {
                val entry = issueQueue(i)
                val idx   = entry.ROB_index

                // Determine if the entry should be invalidated based on ROB pointers and last_index
                // Invalidate if the index is between last_index (inclusive) and tail (exclusive),
                // handling wrap-around.
                val shouldInvalidate = Mux(
                  tail >= head, // Normal case (no wrap-around)
                  idx >= last && idx < tail,
                  // Wrap-around case
                  idx >= last || idx < tail // Invalidate if >= last OR < tail
                )

                // Keep the entry only if it's currently valid AND should NOT be invalidated by recovery
                when(entry.valid && !shouldInvalidate) {
                    // Copy the valid, non-invalidated entry to the next state queue
                    // Use the Scala var to determine the destination index
                    nextIssueQueue(nextValidCount.U) := entry
                    // Propagate the updated ready flags calculated earlier (before selection)
                    nextIssueQueue(nextValidCount.U).rdyL := currentRdyL(i)
                    nextIssueQueue(nextValidCount.U).rdyR := currentRdyR(i)
                    // Ensure valid flag is set correctly
                    nextIssueQueue(nextValidCount.U).valid := true.B
                    nextValidCount += 1
                }
            }
            // Set the number of valid entries after recovery compaction
            nextNumValidEntries := nextValidCount.U
        }.otherwise {
            // clear
            for (i <- 0 until nIQEntries) {
                nextIssueQueue(i).valid := false.B
            }
        }
    }

    // --- Update State Registers ---
    // Commit the next state at the end of the clock cycle
    issueQueue      := nextIssueQueue
    numValidEntries := nextNumValidEntries
    writePtr        := nextNumValidEntries
    regNumIssued := numIssued // Store the number of issued instructions for the next cycle

    io.out.issued0    := regissued0
    io.out.issued1    := regissued1
    io.out.issued2    := regissued2
    io.out.issued3    := regissued3
    io.out.issueValid := issueValid

    // --- Queue Full Logic ---
    // Signal full if fewer than 4 slots will be available *after* potential insertion next cycle.
    // This is predictive based on current state. A simpler version checks current occupancy.
    // Simple version: Full if currently has > nIQEntries-4 entries.
    io.out.queueFull := numValidEntries > (nIQEntries - 4).U
}

object IssueQueue extends App {
    ChiselStage.emitSystemVerilogFile(
      new IssueQueue(nIQEntries = 32, nROBEntries = 32),
      Array("--target-dir", "generated")
    )
}
