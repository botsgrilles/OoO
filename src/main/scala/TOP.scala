// Designed by Kyle. 2025-04-10 14:12
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.io._
import chisel3._
import circt.stage.ChiselStage
import CONSTANTS.CONFIG._
import chisel3.util.is

class TOPDesign extends Module {
    val io = IO(new Bundle {
        val base_ram     = new MySRAMInterface() // 基础RAM接口
        val ext_ram      = new MySRAMInterface() // 扩展RAM接口
        val clock_serial = Input(Clock())        // 时钟输入
        val rxd          = Input(Bool())
        val txd          = Output(Bool())
    })

    // Prefetch & Fetch
    val branchArbiter    = Module(new BranchArbiter())
    val branchPrediction = Module(new BranchPrediction())
    val instRAMInterface = Module(new InstRAMInterface())
    val iCache           = Module(new ICache())
    val sramController   = Module(new SRAMControllerNew())
    // val instBuffer       = Module(new InstBuffer(capacity = 16))
    // Decode
    val pipeline_F_D = Module(new Pipline_F_D())
    val decoderArray = Module(new DecoderArray())

    // Rename
    val pipeline_D_RN = Module(new Pipline_D_RN(combinational = true))
    val registerRenaming = Module(
      new RegisterRenaming(combinational = true, nPsyReg = nPsyRegs)
    )

    // WriteROB
    val pipeline_RN_WR = Module(new Pipline_RN_WR(nPsyRegs))

    // Issue
    val pipeline_WR_I = Module(new Pipline_WR_I(nROBEntries = nROBEntries))
    val issueQueue    = Module(new IssueQueueDistributed(8, 8, 8, nROBEntries, nPsyRegs))

    // PreExecute & Execute
    val PreExecution = Module(new PreExecution(nROBEntries = nROBEntries))
    val registerFile = Module(new RegisterFile(nPsyRegs = nPsyRegs))
    val sourceMuxes  = Module(new SourceMUXes())
    val alu0         = Module(new ALU())
    val alu1         = Module(new ALU())
    val writeBuffer = Module(
      new WriteBuffer(nROBEntries = nROBEntries, nWBEntries = nWBEntries)
    )
    val multiplier = Module(new Multiplier(mode = mode))
    val broadcastWakeupController = Module(
      new BroadcastWakeupController(nPsyReg = nPsyRegs)
    )
    val branchResultController = Module(
      new BranchResultController(nROBEntries = nROBEntries)
    )
    val uartController = Module(new UARTControllerAsync(UARTFIFODepth))

    // Commit
    val pauseResetController = Module(new PauseResetController())
    val reorderBuffer        = Module(new ReorderBuffer(nROBEntries, nPsyRegs = nPsyRegs))

    /* -------------------------------------- */
    /* ---------- Prefetch & Fetch ---------- */
    /* -------------------------------------- */

    // brancbranchArbiter
    branchArbiter.io.in.pausePC_update      := pauseResetController.io.out.pause_PC_update

    branchArbiter.io.in.EX_PC_set           := branchResultController.io.out.mispredict
    branchArbiter.io.in.EX_true_PC          := branchResultController.io.out.target
    branchArbiter.io.in.Decode_PC_set       := pauseResetController.io.out.misPredictD
    branchArbiter.io.in.Decode_true_PC      := decoderArray.io.out.true_PC
    branchArbiter.io.in.direction_predicted := branchPrediction.io.out.direction_predicted
    branchArbiter.io.in.target_predicted    := branchPrediction.io.out.target_predicted

    branchArbiter.io.in.current_PC          := branchArbiter.io.out.PC_arbited
    branchArbiter.io.in.offset_predicted    := branchPrediction.io.out.offset_predicted
    branchArbiter.io.in.condition_predicted := branchPrediction.io.out.condition_predicted
    branchArbiter.io.in.removeMask          := pauseResetController.io.out.removeMask

    // branchPrediction
    branchPrediction.io.in.PC                := branchArbiter.io.out.PC_arbited
    branchPrediction.io.in.PC_where_branched := reorderBuffer.io.out.branch_info.PC_where_branched
    branchPrediction.io.in.update_enable     := reorderBuffer.io.out.branch_info.valid
    branchPrediction.io.in.direction_true    := reorderBuffer.io.out.branch_info.direction_true
    branchPrediction.io.in.target_true       := reorderBuffer.io.out.branch_info.target_true
    branchPrediction.io.in.branch_type       := reorderBuffer.io.out.branch_info.branch_type
    branchPrediction.io.in.pause_PC_update   := pauseResetController.io.out.pause_PC_update

    instRAMInterface.io.in.PC   := branchArbiter.io.out.PC_arbited
    instRAMInterface.io.in.mask := pauseResetController.io.out.mask_inst

    iCache.io.cpu.req_valid               := instRAMInterface.io.icache.req_valid
    iCache.io.cpu.req_addr                := instRAMInterface.io.icache.req_addr
    instRAMInterface.io.icache.resp_valid := iCache.io.cpu.resp_valid
    instRAMInterface.io.icache.resp_data  := iCache.io.cpu.resp_data
    instRAMInterface.io.icache.stall      := iCache.io.cpu.stall
    iCache.iCacheRefresh                  := branchArbiter.io.out.iCache_refresh

    iCache.io.sram.resp_data  := sramController.io.icache_resp.data
    iCache.io.sram.resp_valid := sramController.io.icache_resp.valid

    sramController.io.icache_req.valid := iCache.io.sram.req_valid
    sramController.io.icache_req.addr  := iCache.io.sram.req_addr

    sramController.io.wb_req.valid := writeBuffer.io.out.reqValid
    sramController.io.wb_req.addr  := writeBuffer.io.out.RAMAddr
    sramController.io.wb_req.we    := writeBuffer.io.out.RAMwriteEnable
    sramController.io.wb_req.data  := writeBuffer.io.out.RAMwriteData
    sramController.io.wb_req.be    := writeBuffer.io.out.RAMBen

    sramController.io.base_ram <> io.base_ram
    sramController.io.ext_ram <> io.ext_ram
    sramController.io.uart <> uartController.io.cpu

    io.txd                         := uartController.io.uart.txd
    uartController.io.uart.rxd     := io.rxd
    uartController.io.clock_serial := io.clock_serial
    uartController.io.commit       := reorderBuffer.io.out.uart_commit
    uartController.io.recover      := pauseResetController.io.out.reset_WB

    /* ---------- Decode ---------- */
    pipeline_F_D.io.in.PC_next          := branchArbiter.io.out.PC_arbited
    pipeline_F_D.io.in.clear            := pauseResetController.io.out.reset_FD
    pipeline_F_D.io.in.pause            := pauseResetController.io.out.pause_FD
    pipeline_F_D.io.in.instruction      := instRAMInterface.io.out.Insts
    pipeline_F_D.io.in.offset_predicted := branchArbiter.io.out.offset_predicted
    pipeline_F_D.io.in.predicted_taken  := branchArbiter.io.out.predicted_taken
    pipeline_F_D.io.in.valid            := instRAMInterface.io.out.inst_valid
    pipeline_F_D.io.in.refetch_D        := pauseResetController.io.out.misPredictD

    decoderArray.io.in.PC               := pipeline_F_D.io.out.PC
    decoderArray.io.in.PC_next          := pipeline_F_D.io.out.PC_next
    decoderArray.io.in.predict_taken    := pipeline_F_D.io.out.predicted_taken
    decoderArray.io.in.instruction      := pipeline_F_D.io.out.instruction
    decoderArray.io.in.offset_predicted := pipeline_F_D.io.out.offset_predicted
    decoderArray.io.in.valid            := pipeline_F_D.io.out.valid
    decoderArray.io.in.pause            := pipeline_F_D.io.in.pause

    /* ---------- Rename ---------- */
    pipeline_D_RN.io.in.PC               := pipeline_F_D.io.out.PC
    pipeline_D_RN.io.in.PC_next          := decoderArray.io.out.PC_next
    pipeline_D_RN.io.in.PC_decode        := decoderArray.io.out.true_PC
    pipeline_D_RN.io.in.decode_PC_set    := false.B // no longer used
    pipeline_D_RN.io.in.offset_predicted := decoderArray.io.out.offset_predicted
    pipeline_D_RN.io.in.predicted_taken  := decoderArray.io.out.predict_taken
    pipeline_D_RN.io.in.pause            := pauseResetController.io.out.pause_DRN
    pipeline_D_RN.io.in.clear            := pauseResetController.io.out.reset_DRN

    for (i <- 0 until 4) {
        pipeline_D_RN.io.in.decoded(i).branch_inst := decoderArray.io.out.branch_inst(i)
        pipeline_D_RN.io.in.decoded(i).uiL         := decoderArray.io.out.uiL(i)
        pipeline_D_RN.io.in.decoded(i).uiR         := decoderArray.io.out.uiR(i)
        pipeline_D_RN.io.in.decoded(i).srcL        := decoderArray.io.out.srcL(i)
        pipeline_D_RN.io.in.decoded(i).srcR        := decoderArray.io.out.srcR(i)
        pipeline_D_RN.io.in.decoded(i).valL        := decoderArray.io.out.valL(i)
        pipeline_D_RN.io.in.decoded(i).valR        := decoderArray.io.out.valR(i)
        pipeline_D_RN.io.in.decoded(i).dest        := decoderArray.io.out.dest(i)
        pipeline_D_RN.io.in.decoded(i).FU          := decoderArray.io.out.FU(i)
        pipeline_D_RN.io.in.decoded(i).ALU_OP      := decoderArray.io.out.ALU_OP(i)
        pipeline_D_RN.io.in.decoded(i).mem_size    := decoderArray.io.out.mem_size(i)
        pipeline_D_RN.io.in.decoded(i).reg_write   := decoderArray.io.out.reg_write(i)
        pipeline_D_RN.io.in.decoded(i).mem_write   := decoderArray.io.out.mem_write(i)
        pipeline_D_RN.io.in.decoded(i).mem_read    := decoderArray.io.out.mem_read(i)
    }
    pipeline_D_RN.io.in.valid_F_D     := pipeline_F_D.io.out.valid
    pipeline_D_RN.io.in.valid_Decoder := decoderArray.io.out.valid

    for (i <- 0 until 4) {
        registerRenaming.io.in.src(i).srcL := pipeline_D_RN.io.out.decoded(i).srcL
        registerRenaming.io.in.src(i).srcR := pipeline_D_RN.io.out.decoded(i).srcR
        registerRenaming.io.in.src(i).dest := pipeline_D_RN.io.out.decoded(i).dest
    }

    registerRenaming.io.in.recover       := pauseResetController.io.out.resetRAT
    registerRenaming.io.in.pauseRenaming := pauseResetController.io.out.pause_Renaming
    registerRenaming.io.in.commited      := reorderBuffer.io.out.commit_register
    registerRenaming.io.in.dest_old      := reorderBuffer.io.out.dest_old
    registerRenaming.io.in.dest_origin   := reorderBuffer.io.out.dest_origin
    registerRenaming.io.in.commitEnable  := reorderBuffer.io.out.commit_register_enable
    registerRenaming.io.in.valid         := pipeline_D_RN.io.out.valid

    pipeline_RN_WR.io.in.PC               := pipeline_D_RN.io.out.PC
    pipeline_RN_WR.io.in.PC_next          := pipeline_D_RN.io.out.PC_next
    pipeline_RN_WR.io.in.PC_decode        := pipeline_D_RN.io.out.PC_decode
    pipeline_RN_WR.io.in.decode_PC_set    := pipeline_D_RN.io.out.decode_PC_set
    pipeline_RN_WR.io.in.offset_predicted := pipeline_D_RN.io.out.offset_predicted
    pipeline_RN_WR.io.in.predicted_taken  := pipeline_D_RN.io.out.predicted_taken
    pipeline_RN_WR.io.in.pause            := pauseResetController.io.out.pause_RNWR
    pipeline_RN_WR.io.in.clear            := pauseResetController.io.out.resetRNWR
    // pipeline_RN_WR.io.in.valid            := pipeline_D_RN.io.out.valid_delayed
    pipeline_RN_WR.io.in.valid            := registerRenaming.io.out.valid

    /* ---------- Write ROB ---------- */
    for (i <- 0 until 4) {
        pipeline_RN_WR.io.in.default(i).srcL := registerRenaming.io.out.renamed(i).srcL
        pipeline_RN_WR.io.in.default(i).valL := pipeline_D_RN.io.out.decoded(i).valL
        pipeline_RN_WR.io.in.default(i).srcR := registerRenaming.io.out.renamed(i).srcR
        pipeline_RN_WR.io.in.default(i).valR := pipeline_D_RN.io.out.decoded(i).valR
        pipeline_RN_WR.io.in.default(i).dest := registerRenaming.io.out.renamed(i).dest
        pipeline_RN_WR.io.in.default(i).dest_old := registerRenaming.io.out.dest_old(i)
        pipeline_RN_WR.io.in.default(i).dest_origin := pipeline_D_RN.io.out
            .decoded(i)
            .dest
        pipeline_RN_WR.io.in.default(i).uiL := pipeline_D_RN.io.out.decoded(i).uiL
        pipeline_RN_WR.io.in.default(i).uiR := pipeline_D_RN.io.out.decoded(i).uiR
        pipeline_RN_WR.io.in.default(i).branch_inst := pipeline_D_RN.io.out
            .decoded(i)
            .branch_inst
        pipeline_RN_WR.io.in.default(i).FU     := pipeline_D_RN.io.out.decoded(i).FU
        pipeline_RN_WR.io.in.default(i).ALU_OP := pipeline_D_RN.io.out.decoded(i).ALU_OP
        pipeline_RN_WR.io.in.default(i).mem_size := pipeline_D_RN.io.out
            .decoded(i)
            .mem_size
        pipeline_RN_WR.io.in.default(i).reg_write := pipeline_D_RN.io.out
            .decoded(i)
            .reg_write
        pipeline_RN_WR.io.in.default(i).mem_write := pipeline_D_RN.io.out
            .decoded(i)
            .mem_write
        pipeline_RN_WR.io.in.default(i).mem_read := pipeline_D_RN.io.out
            .decoded(i)
            .mem_read

    }

    for (i <- 0 until 4) {
        reorderBuffer.io.in.records(i).srcL := pipeline_RN_WR.io.out.default(i).srcL
        reorderBuffer.io.in.records(i).valL := pipeline_RN_WR.io.out.default(i).valL
        reorderBuffer.io.in.records(i).srcR := pipeline_RN_WR.io.out.default(i).srcR
        reorderBuffer.io.in.records(i).valR := pipeline_RN_WR.io.out.default(i).valR
        reorderBuffer.io.in.records(i).dest := pipeline_RN_WR.io.out.default(i).dest
        reorderBuffer.io.in.records(i).dest_old := pipeline_RN_WR.io.out
            .default(i)
            .dest_old
        reorderBuffer.io.in.records(i).dest_origin := pipeline_RN_WR.io.out
            .default(i)
            .dest_origin
        reorderBuffer.io.in.records(i).mem_write := pipeline_RN_WR.io.out
            .default(i)
            .mem_write
    }

    for (i <- 0 until 2) {
        reorderBuffer.io.in.branch_info.valid(i) := branchResultController.io.out.valid(i)
        reorderBuffer.io.in.branch_info
            .PC_where_branched(i) := branchResultController.io.out.PC_where_branched(i)
        reorderBuffer.io.in.branch_info.target_true(i) := branchResultController.io.out
            .target_true(i)
        reorderBuffer.io.in.branch_info.direction_true(i) := branchResultController.io.out
            .direction_true(i)
        reorderBuffer.io.in.branch_info.branch_type(i) := branchResultController.io.out
            .branch_type(i)
    }

    reorderBuffer.io.in.uart_data_read    := sramController.io.wb_resp.uart_data_read
    reorderBuffer.io.in.misPredict        := branchResultController.io.out.mispredict
    reorderBuffer.io.in.record_valid      := pipeline_RN_WR.io.out.valid
    reorderBuffer.io.in.pauseWriteROB     := pauseResetController.io.out.pauseWriteROB
    reorderBuffer.io.in.commit_indexes(0) := PreExecution.io.out_alu0.ROB_index_ROB
    reorderBuffer.io.in.commit_indexes(1) := PreExecution.io.out_alu1.ROB_index_ROB
    reorderBuffer.io.in.commit_indexes(2) := PreExecution.io.out_mem.ROB_index_ROB
    reorderBuffer.io.in.commit_indexes(3) := PreExecution.io.out_mul.ROB_index
    reorderBuffer.io.in.commit_enable(0)  := PreExecution.io.out_alu0.valid_ROB
    reorderBuffer.io.in.commit_enable(1)  := PreExecution.io.out_alu1.valid_ROB
    reorderBuffer.io.in.commit_enable(2)  := PreExecution.io.out_mem.valid
    reorderBuffer.io.in.commit_enable(3)  := PreExecution.io.out_mul.valid
    reorderBuffer.io.in.writeBufferFull   := writeBuffer.io.out.bufferFull
    reorderBuffer.io.in.lastValidID       := branchResultController.io.out.lastValidID
    reorderBuffer.io.in.clear             := pauseResetController.io.out.resetROB

    /* ---------- Issue ---------- */
    pipeline_WR_I.io.in.pause := pauseResetController.io.out.pause_WRI
    pipeline_WR_I.io.in.reset := pauseResetController.io.out.reset_WRI
    pipeline_WR_I.io.in.valid := reorderBuffer.io.out.insert_valid
    // pipeline_WR_I.io.in.ROB_head_pointer := reorderBuffer.io.out.head_pointer
    // pipeline_WR_I.io.in.ROB_tail_pointer := reorderBuffer.io.out.tail_pointer

    for (i <- 0 until 4) {
        pipeline_WR_I.io.in.pause           := pauseResetController.io.out.pause_WRI
        pipeline_WR_I.io.in.reset           := pauseResetController.io.out.reset_WRI
        pipeline_WR_I.io.in.default(i).srcL := pipeline_RN_WR.io.out.default(i).srcL
        pipeline_WR_I.io.in.default(i).valL := pipeline_RN_WR.io.out.default(i).valL
        pipeline_WR_I.io.in.default(i).srcR := pipeline_RN_WR.io.out.default(i).srcR
        pipeline_WR_I.io.in.default(i).valR := pipeline_RN_WR.io.out.default(i).valR
        pipeline_WR_I.io.in.default(i).dest := pipeline_RN_WR.io.out.default(i).dest
        pipeline_WR_I.io.in.default(i).uiL  := pipeline_RN_WR.io.out.default(i).uiL
        pipeline_WR_I.io.in.default(i).uiR  := pipeline_RN_WR.io.out.default(i).uiR
        pipeline_WR_I.io.in.default(i).branch_inst := pipeline_RN_WR.io.out
            .default(i)
            .branch_inst
        pipeline_WR_I.io.in.default(i).FU     := pipeline_RN_WR.io.out.default(i).FU
        pipeline_WR_I.io.in.default(i).ALU_OP := pipeline_RN_WR.io.out.default(i).ALU_OP
        pipeline_WR_I.io.in.default(i).reg_write := pipeline_RN_WR.io.out
            .default(i)
            .reg_write
        pipeline_WR_I.io.in.default(i).mem_size := pipeline_RN_WR.io.out
            .default(i)
            .mem_size
        pipeline_WR_I.io.in.default(i).mem_write := pipeline_RN_WR.io.out
            .default(i)
            .mem_write
        pipeline_WR_I.io.in.default(i).mem_read := pipeline_RN_WR.io.out
            .default(i)
            .mem_read
        pipeline_WR_I.io.in.default(i).PC        := pipeline_RN_WR.io.out.PC(i)
        pipeline_WR_I.io.in.default(i).PC_next   := pipeline_RN_WR.io.out.PC_next(i)
        pipeline_WR_I.io.in.default(i).PC_decode := pipeline_RN_WR.io.out.PC_decode(i)
        pipeline_WR_I.io.in.default(i).decode_PC_set := pipeline_RN_WR.io.out
            .decode_PC_set(i)
        pipeline_WR_I.io.in.default(i).offset_predicted := pipeline_RN_WR.io.out
            .offset_predicted(i)
        pipeline_WR_I.io.in.default(i).predicted_taken := pipeline_RN_WR.io.out
            .predicted_taken(i)
    }

    issueQueue.io.in.inst      := pipeline_WR_I.io.out.default
    issueQueue.io.in.instValid := pipeline_WR_I.io.out.valid
    issueQueue.io.in.pause := pauseResetController.io.out.pauseWriteIQ
    issueQueue.io.in.indexes          := reorderBuffer.io.out.record_indexes
    issueQueue.io.in.ROB_head_pointer := reorderBuffer.io.out.head_pointer
    issueQueue.io.in.ROB_tail_pointer := reorderBuffer.io.out.tail_pointer
    issueQueue.io.in.last_index       := branchResultController.io.out.lastValidID
    issueQueue.io.in.recover          := pauseResetController.io.out.recover_IQ
    issueQueue.io.in.clear            := pauseResetController.io.out.reset_IQ
    issueQueue.io.in.commited         := reorderBuffer.io.out.commit_register
    issueQueue.io.in.commitEnable     := reorderBuffer.io.out.commit_register_enable
    issueQueue.io.in.execStall(0)     := broadcastWakeupController.io.out.stallALU0
    issueQueue.io.in.execStall(1)     := broadcastWakeupController.io.out.stallALU1
    issueQueue.io.in.execStall(2)     := broadcastWakeupController.io.out.stallMEM
    issueQueue.io.in.execStall(3)     := broadcastWakeupController.io.out.stallMUL

    /* --------- PreExecute & Execute ---------- */
    PreExecution.io.in_alu0.PC               := issueQueue.io.out.issued0.PC
    PreExecution.io.in_alu0.PC_next          := issueQueue.io.out.issued0.PC_next
    PreExecution.io.in_alu0.PC_decode        := issueQueue.io.out.issued0.PC_decode
    PreExecution.io.in_alu0.decode_PC_set    := issueQueue.io.out.issued0.decode_PC_set
    PreExecution.io.in_alu0.offset_predicted := issueQueue.io.out.issued0.offset_predicted
    PreExecution.io.in_alu0.predicted_taken  := issueQueue.io.out.issued0.predicted_taken
    PreExecution.io.in_alu0.uiL              := issueQueue.io.out.issued0.uiL
    PreExecution.io.in_alu0.uiR              := issueQueue.io.out.issued0.uiR
    PreExecution.io.in_alu0.srcL             := issueQueue.io.out.issued0.srcL
    PreExecution.io.in_alu0.valL             := issueQueue.io.out.issued0.valL
    PreExecution.io.in_alu0.srcR             := issueQueue.io.out.issued0.srcR
    PreExecution.io.in_alu0.valR             := issueQueue.io.out.issued0.valR
    PreExecution.io.in_alu0.dest             := issueQueue.io.out.issued0.dest
    PreExecution.io.in_alu0.reg_write        := issueQueue.io.out.issued0.reg_write
    PreExecution.io.in_alu0.branch_inst      := issueQueue.io.out.issued0.branch_inst
    PreExecution.io.in_alu0.ALU_OP           := issueQueue.io.out.issued0.ALU_OP
    PreExecution.io.in_alu0.ROB_index        := issueQueue.io.out.issued0.ROB_index
    PreExecution.io.in_alu0.valid            := issueQueue.io.out.issueValid(0)
    PreExecution.io.in_alu0.clear            := pauseResetController.io.out.reset_PE

    PreExecution.io.in_alu1.PC               := issueQueue.io.out.issued1.PC
    PreExecution.io.in_alu1.PC_next          := issueQueue.io.out.issued1.PC_next
    PreExecution.io.in_alu1.PC_decode        := issueQueue.io.out.issued1.PC_decode
    PreExecution.io.in_alu1.decode_PC_set    := issueQueue.io.out.issued1.decode_PC_set
    PreExecution.io.in_alu1.offset_predicted := issueQueue.io.out.issued1.offset_predicted
    PreExecution.io.in_alu1.predicted_taken  := issueQueue.io.out.issued1.predicted_taken
    PreExecution.io.in_alu1.uiL              := issueQueue.io.out.issued1.uiL
    PreExecution.io.in_alu1.uiR              := issueQueue.io.out.issued1.uiR
    PreExecution.io.in_alu1.srcL             := issueQueue.io.out.issued1.srcL
    PreExecution.io.in_alu1.valL             := issueQueue.io.out.issued1.valL
    PreExecution.io.in_alu1.srcR             := issueQueue.io.out.issued1.srcR
    PreExecution.io.in_alu1.valR             := issueQueue.io.out.issued1.valR
    PreExecution.io.in_alu1.dest             := issueQueue.io.out.issued1.dest
    PreExecution.io.in_alu1.reg_write        := issueQueue.io.out.issued1.reg_write
    PreExecution.io.in_alu1.branch_inst      := issueQueue.io.out.issued1.branch_inst
    PreExecution.io.in_alu1.ALU_OP           := issueQueue.io.out.issued1.ALU_OP
    PreExecution.io.in_alu1.ROB_index        := issueQueue.io.out.issued1.ROB_index
    PreExecution.io.in_alu1.valid            := issueQueue.io.out.issueValid(1)
    PreExecution.io.in_alu1.clear            := pauseResetController.io.out.reset_PE

    PreExecution.io.in_mem.ROB_index := issueQueue.io.out.issued2.ROB_index
    PreExecution.io.in_mem.srcL      := issueQueue.io.out.issued2.srcL
    PreExecution.io.in_mem.valL      := issueQueue.io.out.issued2.valL
    PreExecution.io.in_mem.srcR      := issueQueue.io.out.issued2.srcR
    PreExecution.io.in_mem.valR      := issueQueue.io.out.issued2.valR
    PreExecution.io.in_mem.dest      := issueQueue.io.out.issued2.dest
    PreExecution.io.in_mem.ui        := issueQueue.io.out.issued2.ui
    PreExecution.io.in_mem.mem_size  := issueQueue.io.out.issued2.mem_size
    PreExecution.io.in_mem.reg_write := issueQueue.io.out.issued2.reg_write
    PreExecution.io.in_mem.mem_write := issueQueue.io.out.issued2.mem_write
    PreExecution.io.in_mem.mem_read  := issueQueue.io.out.issued2.mem_read
    PreExecution.io.in_mem.valid     := issueQueue.io.out.issueValid(2)
    PreExecution.io.in_mem.clear     := pauseResetController.io.out.reset_PE

    PreExecution.io.in_mul.ROB_index := issueQueue.io.out.issued3.ROB_index
    PreExecution.io.in_mul.srcL      := issueQueue.io.out.issued3.srcL
    PreExecution.io.in_mul.srcR      := issueQueue.io.out.issued3.srcR
    PreExecution.io.in_mul.dest      := issueQueue.io.out.issued3.dest
    PreExecution.io.in_mul.reg_write := issueQueue.io.out.issued3.reg_write
    PreExecution.io.in_mul.valid     := issueQueue.io.out.issueValid(3)
    PreExecution.io.in_mul.clear     := pauseResetController.io.out.reset_PE

    PreExecution.io.in_alu0.stall := broadcastWakeupController.io.out.stallALU0
    PreExecution.io.in_alu1.stall := broadcastWakeupController.io.out.stallALU1
    PreExecution.io.in_mem.stall  := broadcastWakeupController.io.out.stallMEM
    PreExecution.io.in_mul.stall  := broadcastWakeupController.io.out.stallMUL

    registerFile.io.in.CommonPorts(0).srcL := PreExecution.io.out_alu0.srcL
    registerFile.io.in.CommonPorts(0).srcR := PreExecution.io.out_alu0.srcR
    registerFile.io.in.CommonPorts(0).dest := PreExecution.io.out_alu0.dest_RegisterFile
    registerFile.io.in.CommonPorts(0).WE   := PreExecution.io.out_alu0.reg_write
    registerFile.io.in.CommonPorts(0).WD   := alu0.io.out.result

    registerFile.io.in.CommonPorts(1).srcL := PreExecution.io.out_alu1.srcL
    registerFile.io.in.CommonPorts(1).srcR := PreExecution.io.out_alu1.srcR
    registerFile.io.in.CommonPorts(1).dest := PreExecution.io.out_alu1.dest_RegisterFile
    registerFile.io.in.CommonPorts(1).WE   := PreExecution.io.out_alu1.reg_write
    registerFile.io.in.CommonPorts(1).WD   := alu1.io.out.result

    registerFile.io.in.CommonPorts(2).srcL := PreExecution.io.out_mem.srcL
    registerFile.io.in.CommonPorts(2).srcR := PreExecution.io.out_mem.srcR
    registerFile.io.in.CommonPorts(2).dest := PreExecution.io.out_mem.dest_RegisterFile
    registerFile.io.in.CommonPorts(2).WD   := writeBuffer.io.out.Rdata
    registerFile.io.in.CommonPorts(2).WE   := PreExecution.io.out_mem.reg_write

    registerFile.io.in.CommonPorts(3).srcL := PreExecution.io.out_mul.srcL
    registerFile.io.in.CommonPorts(3).srcR := PreExecution.io.out_mul.srcR
    registerFile.io.in.CommonPorts(3).dest := PreExecution.io.out_mul.dest_RegisterFile
    registerFile.io.in.CommonPorts(3).WE   := PreExecution.io.out_mul.reg_write
    registerFile.io.in.CommonPorts(3).WD   := multiplier.io.result

    registerFile.io.in.stall(0) := broadcastWakeupController.io.out.stallALU0
    registerFile.io.in.stall(1) := broadcastWakeupController.io.out.stallALU1
    registerFile.io.in.stall(2) := broadcastWakeupController.io.out.stallMEM
    registerFile.io.in.stall(3) := broadcastWakeupController.io.out.stallMUL

    sourceMuxes.io.in.ALU0.uiL := PreExecution.io.out_alu0.uiL
    sourceMuxes.io.in.ALU0.uiR := PreExecution.io.out_alu0.uiR
    sourceMuxes.io.in.ALU0.RDL := registerFile.io.out.CommonReadL(0)
    sourceMuxes.io.in.ALU0.RDR := registerFile.io.out.CommonReadR(0)
    sourceMuxes.io.in.ALU0.choiceL := broadcastWakeupController.io.out.MUX_control.MUX_ALU_0L
    sourceMuxes.io.in.ALU0.choiceR := broadcastWakeupController.io.out.MUX_control.MUX_ALU_0R
    sourceMuxes.io.in.ALU0.stallL := broadcastWakeupController.io.out.muxStallALU0L
    sourceMuxes.io.in.ALU0.stallR := broadcastWakeupController.io.out.muxStallALU0R

    sourceMuxes.io.in.ALU1.uiL := PreExecution.io.out_alu1.uiL
    sourceMuxes.io.in.ALU1.uiR := PreExecution.io.out_alu1.uiR
    sourceMuxes.io.in.ALU1.RDL := registerFile.io.out.CommonReadL(1)
    sourceMuxes.io.in.ALU1.RDR := registerFile.io.out.CommonReadR(1)
    sourceMuxes.io.in.ALU1.choiceL := broadcastWakeupController.io.out.MUX_control.MUX_ALU_1L
    sourceMuxes.io.in.ALU1.choiceR := broadcastWakeupController.io.out.MUX_control.MUX_ALU_1R
    sourceMuxes.io.in.ALU1.stallL := broadcastWakeupController.io.out.muxStallALU1L
    sourceMuxes.io.in.ALU1.stallR := broadcastWakeupController.io.out.muxStallALU1R

    sourceMuxes.io.in.MEM.ui  := PreExecution.io.out_mem.ui
    sourceMuxes.io.in.MEM.RDL := registerFile.io.out.CommonReadL(2)
    sourceMuxes.io.in.MEM.RDR := registerFile.io.out.CommonReadR(2)
    sourceMuxes.io.in.MEM.choiceL := broadcastWakeupController.io.out.MUX_control.MUX_MEM_L
    sourceMuxes.io.in.MEM.choiceR := broadcastWakeupController.io.out.MUX_control.MUX_MEM_R
    sourceMuxes.io.in.MEM.stallL := broadcastWakeupController.io.out.muxStallMemL
    sourceMuxes.io.in.MEM.stallR := broadcastWakeupController.io.out.muxStallMemR

    sourceMuxes.io.in.MUL.RDL := registerFile.io.out.CommonReadL(3)
    sourceMuxes.io.in.MUL.RDR := registerFile.io.out.CommonReadR(3)
    sourceMuxes.io.in.MUL.choiceL := broadcastWakeupController.io.out.MUX_control.MUX_MUL_L
    sourceMuxes.io.in.MUL.choiceR := broadcastWakeupController.io.out.MUX_control.MUX_MUL_R
    sourceMuxes.io.in.MUL.stallL := broadcastWakeupController.io.out.muxStallMULL
    sourceMuxes.io.in.MUL.stallR := broadcastWakeupController.io.out.muxStallMULR

    sourceMuxes.io.in.broadcast(0) := alu0.io.out.result
    sourceMuxes.io.in.broadcast(1) := alu1.io.out.result
    sourceMuxes.io.in.broadcast(2) := writeBuffer.io.out.Rdata
    sourceMuxes.io.in.broadcast(3) := multiplier.io.result

    alu0.io.in.inA    := sourceMuxes.io.out.ALU0.L
    alu0.io.in.inB    := sourceMuxes.io.out.ALU0.R
    alu0.io.in.ALU_OP := PreExecution.io.out_alu0.ALU_OP

    alu1.io.in.inA    := sourceMuxes.io.out.ALU1.L
    alu1.io.in.inB    := sourceMuxes.io.out.ALU1.R
    alu1.io.in.ALU_OP := PreExecution.io.out_alu1.ALU_OP

    writeBuffer.io.in.clear       := pauseResetController.io.out.reset_WB
    writeBuffer.io.in.ROBIndex    := PreExecution.io.out_mem.ROB_index_WB
    writeBuffer.io.in.writeEnable := PreExecution.io.out_mem.mem_write
    writeBuffer.io.in.readEnable  := PreExecution.io.out_mem.mem_read
    writeBuffer.io.in.addr        := sourceMuxes.io.out.MEM.WAddr
    writeBuffer.io.in.Wdata       := sourceMuxes.io.out.MEM.WData
    writeBuffer.io.in.writeSize   := PreExecution.io.out_mem.mem_size

    writeBuffer.io.in.readData      := sramController.io.wb_resp.data
    writeBuffer.io.in.responseValid := sramController.io.wb_resp.valid

    writeBuffer.io.in.retireEnable := reorderBuffer.io.out.writeBufferValid
    writeBuffer.io.in.retireIndex  := reorderBuffer.io.out.writeBufferIndex

    writeBuffer.io.in.uart_data_read := sramController.io.wb_resp.uart_data_read

    multiplier.io.a := sourceMuxes.io.out.MUL.L
    multiplier.io.b := sourceMuxes.io.out.MUL.R

    broadcastWakeupController.io.in.issueValid := issueQueue.io.out.issueValid

    broadcastWakeupController.io.in.regALU0.srcL := PreExecution.io.out_alu0.srcL
    broadcastWakeupController.io.in.regALU0.srcR := PreExecution.io.out_alu0.srcR
    broadcastWakeupController.io.in.regALU0.valL := PreExecution.io.out_alu0.valL
    broadcastWakeupController.io.in.regALU0.valR := PreExecution.io.out_alu0.valR
    broadcastWakeupController.io.in.regALU0.dest := PreExecution.io.out_alu0.dest_BWC

    broadcastWakeupController.io.in.regALU1.srcL := PreExecution.io.out_alu1.srcL
    broadcastWakeupController.io.in.regALU1.srcR := PreExecution.io.out_alu1.srcR
    broadcastWakeupController.io.in.regALU1.valL := PreExecution.io.out_alu1.valL
    broadcastWakeupController.io.in.regALU1.valR := PreExecution.io.out_alu1.valR
    broadcastWakeupController.io.in.regALU1.dest := PreExecution.io.out_alu1.dest_BWC

    broadcastWakeupController.io.in.regMEM.srcL := PreExecution.io.out_mem.srcL
    broadcastWakeupController.io.in.regMEM.srcR := PreExecution.io.out_mem.srcR
    broadcastWakeupController.io.in.regMEM.valL := PreExecution.io.out_mem.valL
    broadcastWakeupController.io.in.regMEM.valR := PreExecution.io.out_mem.valR
    broadcastWakeupController.io.in.regMEM.dest := PreExecution.io.out_mem.dest_BWC

    broadcastWakeupController.io.in.regMUL.srcL := PreExecution.io.out_mul.srcL
    broadcastWakeupController.io.in.regMUL.srcR := PreExecution.io.out_mul.srcR
    broadcastWakeupController.io.in.regMUL.dest := PreExecution.io.out_mul.dest_BWC

    broadcastWakeupController.io.in.cacheMiss := writeBuffer.io.out.cacheMiss

    broadcastWakeupController.io.in.infoALU0 := issueQueue.io.out.regsInfo0
    broadcastWakeupController.io.in.infoALU1 := issueQueue.io.out.regsInfo1
    broadcastWakeupController.io.in.infoMEM := issueQueue.io.out.regsInfo2
    broadcastWakeupController.io.in.infoMUL := issueQueue.io.out.regsInfo3
    broadcastWakeupController.io.in.validWire := issueQueue.io.out.validWire

    branchResultController.io.in.valid0         := PreExecution.io.out_alu0.valid_BRC
    branchResultController.io.in.valid1         := PreExecution.io.out_alu1.valid_BRC
    branchResultController.io.in.PC0            := PreExecution.io.out_alu0.PC
    branchResultController.io.in.PC1            := PreExecution.io.out_alu1.PC
    branchResultController.io.in.PC_predicted0  := PreExecution.io.out_alu0.PC_next
    branchResultController.io.in.PC_predicted1  := PreExecution.io.out_alu1.PC_next
    branchResultController.io.in.PC_decode0     := PreExecution.io.out_alu0.PC_decode
    branchResultController.io.in.PC_decode1     := PreExecution.io.out_alu1.PC_decode
    branchResultController.io.in.decode_PC_set0 := PreExecution.io.out_alu0.decode_PC_set
    branchResultController.io.in.decode_PC_set1 := PreExecution.io.out_alu1.decode_PC_set
    branchResultController.io.in.offset_predicted0 := PreExecution.io.out_alu0.offset_predicted
    branchResultController.io.in.offset_predicted1 := PreExecution.io.out_alu1.offset_predicted
    branchResultController.io.in.predicted_taken0 := PreExecution.io.out_alu0.predicted_taken
    branchResultController.io.in.predicted_taken1 := PreExecution.io.out_alu1.predicted_taken
    branchResultController.io.in.ZF0         := alu0.io.out.ZF
    branchResultController.io.in.SF0         := alu0.io.out.SF
    branchResultController.io.in.ZF1         := alu1.io.out.ZF
    branchResultController.io.in.SF1         := alu1.io.out.SF
    branchResultController.io.in.F0          := alu0.io.out.result
    branchResultController.io.in.F1          := alu1.io.out.result
    branchResultController.io.in.ROBIndex0   := PreExecution.io.out_alu0.ROB_index_BRC
    branchResultController.io.in.ROBIndex1   := PreExecution.io.out_alu1.ROB_index_BRC
    branchResultController.io.in.ROBHead     := reorderBuffer.io.out.head_pointer
    branchResultController.io.in.ROBTail     := reorderBuffer.io.out.tail_pointer
    branchResultController.io.in.branchInst0 := PreExecution.io.out_alu0.branch_inst
    branchResultController.io.in.branchInst1 := PreExecution.io.out_alu1.branch_inst
    branchResultController.io.in.uiL0        := PreExecution.io.out_alu0.uiL_BRC
    branchResultController.io.in.uiL1        := PreExecution.io.out_alu1.uiL_BRC
    branchResultController.io.in.uiR0        := PreExecution.io.out_alu0.uiR_BRC
    branchResultController.io.in.uiR1        := PreExecution.io.out_alu1.uiR_BRC
    branchResultController.io.in.regRead0    := sourceMuxes.io.out.ALU0.R
    branchResultController.io.in.regRead1    := sourceMuxes.io.out.ALU1.R
    branchResultController.io.in.destReg0    := PreExecution.io.out_alu0.dest_RegisterFile
    branchResultController.io.in.destReg1    := PreExecution.io.out_alu1.dest_RegisterFile
    branchResultController.io.in.RemoveMask  := pauseResetController.io.out.removeMask

    /* ---------- Commit ---------- */
    pauseResetController.io.in.reFetchDecoder := decoderArray.io.out.reFetch
    pauseResetController.io.in.RATfull    := registerRenaming.io.out.freeListFull
    pauseResetController.io.in.queueFull  := issueQueue.io.out.queueFull
    pauseResetController.io.in.ROBFull    := reorderBuffer.io.out.ROB_full
    pauseResetController.io.in.ROBEmpty   := reorderBuffer.io.out.ROB_empty
    pauseResetController.io.in.misPredict := branchResultController.io.out.mispredict
    pauseResetController.io.in.cacheStallRequest := instRAMInterface.io.out.cacheStallRequest

    println(s"UART will be running at baud rate: \u001B[33m${BaudRate}\u001B[0m. FIFO Depth: \u001B[33m${UARTFIFODepth}\u001B[0m.\n")
}
