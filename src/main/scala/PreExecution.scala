// Designed by Kyle. 2025-03-14 19:32
import chisel3._
import circt.stage.ChiselStage
import chisel3.util.log2Up
import chisel3.util.ShiftRegister
import CONSTANTS._
import CONSTANTS.CONFIG.nPsyRegs

/*
    预执行模块
    按不同延迟发送各个值，保证在合适的时期向执行电路提供数据
    对接 `Broadcast Wakeup Controller` 和 `Branch Result Controller`
    - 直通 (Wire): srcL[6:0], srcR[6:0], ROB_index, valL, valR
    - 延迟一周期：uiL[31:0], uiR[19:0], ALU_OP[3:0], reg_write(ALU), mem_write
    - 延迟两个周期： reg_write(MEM), dest[6:0](ALU), PC[31:0], PC_next[31:0], valid (to BranchResultController)
    - 延迟三个周期：ROB_index(ALU), dest[6:0](MUL), reg_write(MUL), dest[6:0](MEM)
    - 延迟四个周期：ROB_index(MUL)
    - 延迟五个周期：ROB_index(MUL)
    - TODO: 加入DCache后，MEM的延迟不定，需要根据 `cache_miss` 信号决定
 */

class IssueInstALU(nROBEntries: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val valid            = Input(Bool())
            val stall            = Input(Bool())
            val PC               = Input(UInt(32.W))
            val PC_next          = Input(UInt(32.W))
            val PC_decode        = Input(UInt(32.W))
            val decode_PC_set    = Input(Bool())
            val offset_predicted = Input(UInt(2.W))
            val predicted_taken  = Input(Bool()) // 分支预测器的预测结果
            val uiL              = Input(UInt(32.W))
            val uiR              = Input(UInt(32.W))
            val srcL             = Input(UInt(log2Up(nPsyRegs).W))
            val valL             = Input(Bool())
            val srcR             = Input(UInt(log2Up(nPsyRegs).W))
            val valR             = Input(Bool())
            val dest             = Input(UInt(log2Up(nPsyRegs).W))
            val reg_write        = Input(UInt(4.W))
            val branch_inst      = Input(UInt(4.W))
            val ALU_OP           = Input(UInt(4.W))
            val ROB_index        = Input(UInt(log2Up(nROBEntries).W))

            val clear = Input(Bool()) // Clear signal to reset the module
        }
        val out = new Bundle {
            val valid_ROB         = Output(Bool())
            val valid_BRC         = Output(Bool())
            val srcL              = Output(UInt(log2Up(nPsyRegs).W)) // 同时发送给 RegisterFile 和 BWC
            val valL              = Output(Bool())
            val srcR              = Output(UInt(log2Up(nPsyRegs).W))
            val valR              = Output(Bool())
            val PC                = Output(UInt(32.W))
            val PC_next           = Output(UInt(32.W))
            val PC_decode         = Output(UInt(32.W))
            val decode_PC_set     = Output(Bool())
            val offset_predicted  = Output(UInt(2.W))
            val predicted_taken   = Output(Bool()) // 分支预测器的预测结果
            val dest_RegisterFile = Output(UInt(log2Up(nPsyRegs).W))
            val dest_BWC          = Output(UInt(log2Up(nPsyRegs).W))
            val uiL               = Output(UInt(32.W))
            val uiL_BRC           = Output(UInt(32.W))
            val uiR_BRC           = Output(UInt(32.W))
            val uiR               = Output(UInt(32.W))
            val ALU_OP            = Output(UInt(4.W))
            val reg_write         = Output(Bool())
            val branch_inst       = Output(UInt(4.W))
            val ROB_index_BRC     = Output(UInt(log2Up(nROBEntries).W))
            val ROB_index_ROB     = Output(UInt(log2Up(nROBEntries).W))
        }
    })
    // Direct wire connections (no delay)
    io.out.srcL     := io.in.srcL
    io.out.srcR     := io.in.srcR
    io.out.dest_BWC := io.in.dest
    io.out.valL     := io.in.valL // To BWC
    io.out.valR     := io.in.valR

    // 1-cycle delay signals
    val uiL_reg    = RegInit(0.U(32.W))
    val uiR_reg    = RegInit(0.U(32.W))
    val ALU_OP_reg = RegInit(0.U(4.W))

    val clearMask = RegInit(false.B)
    val clearCounter = RegInit(0.U(2.W))
    val shouldInvalidate = WireInit(false.B)

    when(clearMask){
        shouldInvalidate := true.B
        when (clearCounter === 2.U) {
            clearMask := false.B
        }.otherwise{
            clearCounter := clearCounter + 1.U
        }
    }.otherwise{
        when(io.in.clear || reset.asBool) {
            clearMask := true.B
            clearCounter := 0.U
            shouldInvalidate := true.B
        }
    }

    uiL_reg    := Mux(io.in.stall, uiL_reg, io.in.uiL)
    uiR_reg    := Mux(io.in.stall, uiR_reg, io.in.uiR)
    ALU_OP_reg := Mux(io.in.stall, ALU_OP_reg, io.in.ALU_OP)

    io.out.uiL    := uiL_reg
    io.out.uiR    := uiR_reg
    io.out.ALU_OP := ALU_OP_reg

    // 2-cycle delay signals
    val stall_reg  = RegNext(io.in.stall, false.B)
    val stall_reg2 = RegNext(stall_reg, false.B)

    // 关于stall：放走第一个信号（写寄存器，卡住将要执行的指令）
    val dest_reg2          = ShiftRegister(io.in.dest, 2, 0.U, !io.in.stall || shouldInvalidate)
    val reg_write_reg2     = ShiftRegister(io.in.reg_write, 2, 0.U, !io.in.stall || shouldInvalidate)
    val PC_reg2            = ShiftRegister(io.in.PC, 2, 0.U, !io.in.stall || shouldInvalidate)
    val PC_next_reg2       = ShiftRegister(io.in.PC_next, 2, 0.U, !io.in.stall || shouldInvalidate)
    val PC_decode_reg2     = ShiftRegister(io.in.PC_decode, 2, 0.U, !io.in.stall || shouldInvalidate)
    val decode_PC_set_reg2 = ShiftRegister(io.in.decode_PC_set, 2, false.B, !io.in.stall || shouldInvalidate)
    val offset_predicted_reg2 =
        ShiftRegister(io.in.offset_predicted, 2, 0.U, !io.in.stall || shouldInvalidate)
    val predicted_taken_reg2 =
        ShiftRegister(io.in.predicted_taken, 2, false.B, !io.in.stall || shouldInvalidate)
    val ROB_reg2         = ShiftRegister(io.in.ROB_index, 2, 0.U, !io.in.stall || shouldInvalidate)
    val valid_reg2       = ShiftRegister(io.in.valid, 2, false.B, !io.in.stall || shouldInvalidate)
    val uiL_reg2         = ShiftRegister(io.in.uiL, 2, 0.U, !io.in.stall || shouldInvalidate)
    val uiR_reg2         = ShiftRegister(io.in.uiR, 2, 0.U, !io.in.stall || shouldInvalidate)
    val branch_inst_reg2 = ShiftRegister(io.in.branch_inst, 2, 0.U, !io.in.stall || shouldInvalidate)

    io.out.dest_RegisterFile := Mux(stall_reg, 0.U, dest_reg2)
    io.out.reg_write         := Mux(stall_reg, false.B, reg_write_reg2)
    io.out.PC                := Mux(stall_reg, 0.U, PC_reg2)
    io.out.PC_next           := Mux(stall_reg, 0.U, PC_next_reg2)
    io.out.PC_decode         := Mux(stall_reg, 0.U, PC_decode_reg2)
    io.out.offset_predicted  := Mux(stall_reg, 0.U, offset_predicted_reg2)
    io.out.predicted_taken   := Mux(stall_reg, false.B, predicted_taken_reg2)
    io.out.decode_PC_set     := Mux(stall_reg, false.B, decode_PC_set_reg2)
    io.out.ROB_index_BRC     := Mux(stall_reg, 0.U, ROB_reg2)
    io.out.valid_BRC         := Mux(stall_reg, false.B, valid_reg2)
    io.out.uiL_BRC           := Mux(stall_reg, 0.U, uiL_reg2)
    io.out.uiR_BRC           := Mux(stall_reg, 0.U, uiR_reg2)
    io.out.branch_inst       := Mux(stall_reg, 0.U, branch_inst_reg2)

    // 3-cycle delay signals
    val ROB_index_regn = ShiftRegister(io.in.ROB_index, 3, 0.U, !stall_reg || shouldInvalidate)
    val valid_regn     = ShiftRegister(io.in.valid, 3, false.B, !stall_reg || shouldInvalidate)
    io.out.ROB_index_ROB := Mux(stall_reg2, 0.U, ROB_index_regn)
    io.out.valid_ROB     := Mux(stall_reg2, 0.U, valid_regn)

    when(shouldInvalidate) {
        io.out.srcL     := 0.U
        io.out.srcR     := 0.U
        io.out.dest_BWC := 0.U
        io.out.valL     := false.B
        io.out.valR     := false.B

        uiL_reg    := 0.U
        uiR_reg    := 0.U
        ALU_OP_reg := 0.U

        stall_reg  := false.B
        stall_reg2 := false.B

        dest_reg2             := 0.U
        reg_write_reg2        := false.B
        PC_reg2               := 0.U
        PC_next_reg2          := 0.U
        PC_decode_reg2        := 0.U
        decode_PC_set_reg2    := false.B
        offset_predicted_reg2 := 0.U
        predicted_taken_reg2  := false.B

        ROB_index_regn := 0.U
        valid_regn     := false.B
    }
}

class IssueInstMEM(nROBEntries: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val valid     = Input(Bool())
            val stall     = Input(Bool())
            val ROB_index = Input(UInt(log2Up(nROBEntries).W))
            val srcL      = Input(UInt(log2Up(nPsyRegs).W))
            val valL      = Input(Bool())
            val srcR      = Input(UInt(log2Up(nPsyRegs).W))
            val valR      = Input(Bool())
            val dest      = Input(UInt(log2Up(nPsyRegs).W))
            val ui        = Input(UInt(32.W))
            val mem_size  = Input(UInt(2.W))
            val reg_write = Input(Bool())
            val mem_write = Input(Bool())
            val mem_read  = Input(Bool())

            val clear = Input(Bool()) // Clear signal to reset the module
        }
        val out = new Bundle {
            val valid             = Output(Bool())
            val srcL              = Output(UInt(log2Up(nPsyRegs).W))
            val valL              = Output(Bool())
            val srcR              = Output(UInt(log2Up(nPsyRegs).W))
            val valR              = Output(Bool())
            val dest_BWC          = Output(UInt(log2Up(nPsyRegs).W))
            val dest_RegisterFile = Output(UInt(log2Up(nPsyRegs).W))
            val ui                = Output(UInt(32.W))
            val mem_size          = Output(UInt(2.W))
            val reg_write         = Output(Bool())
            val mem_write         = Output(Bool())
            val mem_read          = Output(Bool())
            val ROB_index_WB      = Output(UInt(log2Up(nROBEntries).W))
            val ROB_index_ROB     = Output(UInt(log2Up(nROBEntries).W))
        }
    })

    val clearMask = RegInit(false.B)
    val clearCounter = RegInit(0.U(2.W))
    val shouldInvalidate = WireInit(false.B)

    when(clearMask){
        shouldInvalidate := true.B
        when (clearCounter === 2.U) {
            clearMask := false.B
        }.otherwise{
            clearCounter := clearCounter + 1.U
        }
    }.otherwise{
        when(io.in.clear || reset.asBool) {
            clearMask := true.B
            clearCounter := 0.U
            shouldInvalidate := true.B
        }
    }

    // Direct wire connections (no delay)
    io.out.srcL     := io.in.srcL
    io.out.srcR     := io.in.srcR
    io.out.valL     := io.in.valL
    io.out.valR     := io.in.valR
    io.out.dest_BWC := io.in.dest

    val stall_reg  = RegNext(io.in.stall, false.B)
    val stall_reg2 = RegNext(stall_reg, false.B)

    // 1-cycle delay signals
    val uiL_reg       = RegInit(0.U(32.W))
    val mem_write_reg = RegInit(false.B)
    val mem_read_reg  = RegInit(false.B) // Added mem_read
    val ROB_index_reg = RegInit(0.U(io.in.ROB_index.getWidth.W))
    val mem_size_reg  = RegInit(0.U(2.W))

    uiL_reg       := Mux(io.in.stall, uiL_reg, io.in.ui)
    mem_write_reg := Mux(io.in.stall, mem_write_reg, io.in.mem_write)
    mem_read_reg  := Mux(io.in.stall, mem_read_reg, io.in.mem_read)
    ROB_index_reg := Mux(io.in.stall, ROB_index_reg, io.in.ROB_index)
    mem_size_reg  := Mux(io.in.stall, mem_size_reg, io.in.mem_size)

    io.out.ui           := Mux(io.in.stall, 0.U, uiL_reg)
    io.out.mem_write    := Mux(io.in.stall, false.B, mem_write_reg)
    io.out.ROB_index_WB := Mux(io.in.stall, 0.U, ROB_index_reg)
    io.out.mem_size     := Mux(io.in.stall, 0.U, mem_size_reg)
    io.out.mem_read     := Mux(io.in.stall, false.B, mem_read_reg)

    // 2-cycle delay signals
    val dest_reg2      = ShiftRegister(io.in.dest, 2, 0.U, !io.in.stall || shouldInvalidate)
    val reg_write_reg2 = ShiftRegister(io.in.reg_write, 2, false.B, !io.in.stall || shouldInvalidate)
    io.out.dest_RegisterFile := Mux(io.in.stall, 0.U, dest_reg2)
    io.out.reg_write         := Mux(io.in.stall, false.B, reg_write_reg2)

    // 3-cycle delay signals
    val ROB_index_regn = ShiftRegister(io.in.ROB_index, 3, 0.U, !stall_reg || shouldInvalidate)
    val valid_regn     = ShiftRegister(io.in.valid, 3, false.B, !stall_reg || shouldInvalidate)
    io.out.ROB_index_ROB := Mux(stall_reg, 0.U, ROB_index_regn)
    io.out.valid         := Mux(stall_reg, false.B, valid_regn)

    when(shouldInvalidate) {
        io.out.srcL     := 0.U
        io.out.srcR     := 0.U
        io.out.valL     := false.B
        io.out.valR     := false.B
        io.out.dest_BWC := 0.U
        stall_reg       := false.B
        stall_reg2      := false.B

        uiL_reg       := 0.U
        mem_write_reg := 0.U
        mem_read_reg  := 0.U
        ROB_index_reg := 0.U
        mem_size_reg  := 0.U
        dest_reg2     := 0.U

        reg_write_reg2 := false.B
    }
}

class IssueInstMUL(nROBEntries: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val valid     = Input(Bool())
            val stall     = Input(Bool())
            val ROB_index = Input(UInt(log2Up(nROBEntries).W))
            val srcL      = Input(UInt(log2Up(nPsyRegs).W))
            val srcR      = Input(UInt(log2Up(nPsyRegs).W))
            val dest      = Input(UInt(log2Up(nPsyRegs).W))
            val reg_write = Input(Bool())

            val clear = Input(Bool()) // Clear signal to reset the module
        }
        val out = new Bundle {
            val valid             = Output(Bool())
            val ROB_index         = Output(UInt(log2Up(nROBEntries).W))
            val srcL              = Output(UInt(log2Up(nPsyRegs).W))
            val srcR              = Output(UInt(log2Up(nPsyRegs).W))
            val dest_BWC          = Output(UInt(log2Up(nPsyRegs).W))
            val dest_RegisterFile = Output(UInt(log2Up(nPsyRegs).W))
            val reg_write         = Output(Bool())
        }
    })

    val clearMask = RegInit(false.B)
    val clearCounter = RegInit(0.U(2.W))
    val shouldInvalidate = WireInit(false.B)

    when(clearMask){
        shouldInvalidate := true.B
        when (clearCounter === 2.U) {
            clearMask := false.B
        }.otherwise{
            clearCounter := clearCounter + 1.U
        }
    }.otherwise{
        when(io.in.clear || reset.asBool) {
            clearMask := true.B
            clearCounter := 0.U
            shouldInvalidate := true.B
        }
    }

    io.out.srcL     := io.in.srcL
    io.out.srcR     := io.in.srcR
    io.out.dest_BWC := io.in.dest

    val stall_reg  = ShiftRegister(io.in.stall, EXECUTION_CYCLE.MUL, false.B, true.B)
    val stall_reg2 = RegNext(stall_reg, false.B)

    val reg_write_reg =
        ShiftRegister(io.in.reg_write, EXECUTION_CYCLE.MUL + 1, 0.U, !io.in.stall || shouldInvalidate)
    val dest_reg = ShiftRegister(io.in.dest, EXECUTION_CYCLE.MUL + 1, 0.U, !io.in.stall || shouldInvalidate)

    io.out.reg_write         := Mux(stall_reg, 0.U, reg_write_reg)
    io.out.dest_RegisterFile := Mux(stall_reg, 0.U, dest_reg)

    val ROB_index_regn =
        ShiftRegister(io.in.ROB_index, EXECUTION_CYCLE.MUL + 2, 0.U, !stall_reg || shouldInvalidate)
    val valid_regn =
        ShiftRegister(io.in.valid, EXECUTION_CYCLE.MUL + 2, false.B, !stall_reg || shouldInvalidate)

    io.out.ROB_index := Mux(stall_reg2, 0.U, ROB_index_regn)
    io.out.valid     := Mux(stall_reg2, false.B, valid_regn)

    when(shouldInvalidate) {
        io.out.srcL     := 0.U
        io.out.srcR     := 0.U
        io.out.dest_BWC := 0.U

        stall_reg      := false.B
        stall_reg2     := false.B
        reg_write_reg  := false.B
        dest_reg       := 0.U
        ROB_index_regn := 0.U
        valid_regn     := false.B
    }
}

class PreExecution(nROBEntries: Int) extends Module {
    val issueInstALU0 = Module(new IssueInstALU(nROBEntries))
    val issueInstALU1 = Module(new IssueInstALU(nROBEntries))
    val issueInstMEM  = Module(new IssueInstMEM(nROBEntries))
    val issueInstMUL  = Module(new IssueInstMUL(nROBEntries))

    val io = IO(new Bundle {
        val in_alu0  = Input(issueInstALU0.io.in.cloneType)
        val in_alu1  = Input(issueInstALU1.io.in.cloneType)
        val in_mem   = Input(issueInstMEM.io.in.cloneType)
        val in_mul   = Input(issueInstMUL.io.in.cloneType)
        val out_alu0 = Output(issueInstALU0.io.out.cloneType)
        val out_alu1 = Output(issueInstALU1.io.out.cloneType)
        val out_mem  = Output(issueInstMEM.io.out.cloneType)
        val out_mul  = Output(issueInstMUL.io.out.cloneType)
    })

    issueInstALU0.io.in := io.in_alu0
    io.out_alu0         := issueInstALU0.io.out

    issueInstALU1.io.in := io.in_alu1
    io.out_alu1         := issueInstALU1.io.out

    issueInstMEM.io.in := io.in_mem
    io.out_mem         := issueInstMEM.io.out

    issueInstMUL.io.in := io.in_mul
    io.out_mul         := issueInstMUL.io.out
}

object PreExecution extends App {
    ChiselStage.emitSystemVerilogFile(
      new PreExecution(32),
      Array("--target-dir", "generated")
    )
}
