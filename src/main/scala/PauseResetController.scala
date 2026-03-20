// Designed by Kyle. 2025-04-03 14:19
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import CONSTANTS.CONFIG.advancedDebug
import CONSTANTS.CONFIG.debugSignal
/*
    全局流水线暂停与冲刷控制
    - if RATFull: pause {PC_update, F/D, D/RN, Renaming}
    - if queueFull: pause {PC_update, F/D, D/RN, Renaming, RN/I}
    - if ROBFull: pause {PC_update, F/D, D/RN, Renaming}
    - if mispredict:
        reset (F/D, D/RN),
        - wait 2 clocks (when new instruction finishes decoding) then pause {PC_update, F/D, D/RN},
        - meanwhile pause {Renaming, RN/I}
        -> until ROBEmpty, reset {RAT, ROB, RN/I, IssueQueue, WriteBuffer}, remove all pauses
 */

class PauseResetController extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val RATfull           = Input(Bool()) // RegisterRenaming
            val queueFull         = Input(Bool()) // IssueQueue
            val ROBFull           = Input(Bool()) // ROB
            val ROBEmpty          = Input(Bool()) // ROB
            val misPredict        = Input(Bool()) // PredictionUnit
            val cacheStallRequest = Input(Bool()) // Cache是否stall
            val reFetchDecoder    = Input(Bool())
        }
        val out = new Bundle {
            val pause_PC_update = Output(Bool()) // BPU
            val pause_FD        = Output(Bool()) // F/D
            val misPredictD     = Output(Bool())
            val pause_DRN       = Output(Bool()) // D/RN
            val pause_RNWR      = Output(Bool()) // RN/WR
            val pauseWriteROB   = Output(Bool()) // WriteBuffer
            val pause_WRI       = Output(Bool())
            val pause_Renaming  = Output(Bool()) // RegisterRenaming
            val pauseWriteIQ    = Output(Bool()) // IssueQueue

            val reset_FD   = Output(Bool()) // F/D
            val reset_DRN  = Output(Bool())
            val resetRAT   = Output(Bool()) // RegisterRenaming (RAT)
            val resetROB   = Output(Bool()) // ROB
            val resetRNWR  = Output(Bool()) // RN/I
            val reset_WRI  = Output(Bool())
            val recover_IQ = Output(Bool()) // IssueQueue
            val reset_IQ   = Output(Bool()) // IssueQueue
            val reset_WB   = Output(Bool()) // WriteBuffer
            val reset_PE   = Output(Bool()) // PreExecution

            val mask_inst = Output(Bool()) // InstRAMInterface

            val removeMask = Output(Bool()) // BranchResultController
        }
    })
    // Pause signals
    val mispredict_state = RegInit(false.B)

    // Default values
    io.out.pause_PC_update := false.B
    io.out.pause_FD        := false.B
    io.out.pause_DRN       := false.B
    io.out.pause_RNWR      := false.B
    io.out.pauseWriteROB   := false.B
    io.out.pause_WRI       := false.B
    io.out.pause_Renaming  := false.B
    io.out.pauseWriteIQ    := false.B

    io.out.reset_FD  := false.B
    io.out.reset_DRN := false.B
    io.out.resetRAT  := false.B
    io.out.resetROB  := false.B
    io.out.resetRNWR := false.B
    io.out.reset_WRI := false.B
    io.out.reset_IQ  := false.B
    io.out.reset_WB  := false.B
    io.out.reset_PE  := false.B
    io.out.misPredictD := false.B

    io.out.recover_IQ := false.B
    io.out.removeMask := false.B

    // io.out.mask_inst := RegNext(RegNext(io.out.pause_PC_update, false.B), false.B)
    io.out.mask_inst := false.B

    // Mispredict state machine
    // States: 0 = idle, 1 = reset F/D, D/RN, 2 = wait 2 cycles, 3 = pause PC_update/F/D/D/RN, pause Renaming/RNWR, wait ROBEmpty, 4 = reset RAT/ROB/RNWR/IssueQueue/WriteBuffer
    val sIdle :: sWait2 :: sPauseAndWaitROBEmpty :: sResetAll :: Nil =
        Enum(4)
    val mispredictState = RegInit(sIdle)
    val waitCounter     = RegInit(0.U(2.W)) // 2-bit counter for 2 cycles

    def misPredictDetected(): Unit = {
        io.out.reset_FD       := true.B
        io.out.reset_DRN      := true.B
        io.out.pauseWriteROB  := true.B
        io.out.pause_Renaming := true.B
        io.out.resetRNWR      := true.B
        io.out.reset_WRI      := true.B
        io.out.recover_IQ     := true.B
        waitCounter           := 0.U
    }

    // If it takes too long(or forever) to recover, then ILA is triggered
    if (advancedDebug) {
        val recoverCounter = RegInit(0.U(8.W))
        when(mispredictState =/= sIdle) {
            recoverCounter := Mux(recoverCounter === 255.U, 255.U, recoverCounter + 1.U)
        }.otherwise {
            recoverCounter := 0.U
        }

        debugSignal(recoverCounter)
    }

    switch(mispredictState) {
        is(sIdle) {
            when(io.in.misPredict) {
                misPredictDetected()
                mispredictState := sWait2
            }
        }
        is(sWait2) {
            // io.out.pause_PC_update := true.B
            io.out.pause_Renaming := true.B
            io.out.pause_RNWR     := true.B
            io.out.pauseWriteROB  := true.B
            io.out.pause_WRI      := true.B
            io.out.recover_IQ     := true.B
            waitCounter           := waitCounter + 1.U
            when(io.in.misPredict) {
                misPredictDetected()
                mispredictState := sWait2
            }.elsewhen(waitCounter === 2.U) {
                mispredictState := Mux(io.in.ROBEmpty, sResetAll, sPauseAndWaitROBEmpty)
            }
        }
        is(sPauseAndWaitROBEmpty) {
            io.out.pause_PC_update := true.B
            io.out.pause_FD        := true.B
            io.out.pause_DRN       := true.B
            io.out.pause_Renaming  := true.B
            io.out.pause_RNWR      := true.B
            io.out.pauseWriteROB   := true.B
            io.out.pause_WRI       := true.B
            io.out.recover_IQ      := true.B
            when(io.in.misPredict) {
                misPredictDetected()
                mispredictState := sWait2
            }.elsewhen(io.in.ROBEmpty) {
                mispredictState := sResetAll
            }
        }
        is(sResetAll) {
            io.out.pause_PC_update := true.B
            io.out.pause_FD        := true.B
            io.out.pause_DRN       := true.B
            io.out.pause_Renaming  := true.B
            io.out.pauseWriteROB   := true.B
            io.out.resetRAT        := true.B
            io.out.resetROB        := true.B
            io.out.resetRNWR       := true.B
            io.out.reset_IQ        := true.B
            io.out.reset_WB        := true.B
            io.out.reset_WRI       := true.B
            io.out.reset_PE        := true.B // Reset PreExecution
            io.out.removeMask      := true.B // Clear the mask
            when(io.in.misPredict) {
                misPredictDetected()
                mispredictState := sWait2
            }.otherwise {
                mispredictState := sIdle
            }
        }
    }

    when(io.in.cacheStallRequest) {
        io.out.pause_PC_update := true.B
    }

    // RATfull: pause PC_update, F/D, D/RN, Renaming
    when(io.in.RATfull) {
        io.out.pause_PC_update := true.B
        io.out.pause_FD        := true.B
        io.out.pause_DRN       := true.B
        io.out.pause_Renaming  := true.B
    }

    val sIdle2 :: sPause :: sUpdate :: Nil = Enum(3)

    val queueFullState = RegInit(sIdle2)
    // queueFull: pause PC_update, F/D, D/RN, Renaming, RN/WR

    when(io.in.queueFull && mispredictState === sIdle) {
        io.out.pause_PC_update := true.B
        io.out.pause_FD        := true.B
        io.out.pause_DRN       := true.B
        io.out.pause_Renaming  := true.B
        io.out.pause_RNWR      := true.B
        io.out.pauseWriteROB   := true.B
        io.out.pause_WRI       := true.B
        // io.out.pauseWriteIQ    := true.B // IssueQueue pause
    }

    val ROBFullState = RegInit(sIdle2)
    // ROBFull: pause PC_update, F/D, D/RN, Renaming

    when(io.in.ROBFull && mispredictState === sIdle && !io.in.misPredict) {
        io.out.pause_PC_update := true.B
        io.out.pause_FD        := true.B
        io.out.pause_DRN       := true.B
        io.out.pause_Renaming  := true.B
        io.out.pause_RNWR      := true.B
        io.out.pauseWriteROB   := true.B
        // io.out.pause_WRI       := true.B
        // io.out.pauseWriteIQ    := true.B // IssueQueue pause
    }

    when(!io.in.ROBFull && !io.in.queueFull) {
        io.out.misPredictD := io.in.reFetchDecoder
    }
}

object PauseResetController extends App {
    ChiselStage.emitSystemVerilogFile(
      new PauseResetController(),
      Array("--target-dir", "generated")
    )
}
