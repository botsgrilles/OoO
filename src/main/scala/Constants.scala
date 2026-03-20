import chisel3._

import circt.stage.ChiselStage

object CONSTANTS {
    object CONFIG {
        val DEBUG_SIGNALS = true
        // val mode          = "Simulation"
        val UARTFIFODepth = 16
        val BaudRate               = 9600
        val mode          = "Synthesis"
        val PERF_STATS    = true // 是否开启性能统计
        val nROBEntries   = 32
        val nIQEntries    = 32
        val nWBEntries    = 8
        val nPsyRegs      = 64
        val combinational          = true
        val DataRAMSize            = 4 * 1024 * 1024 // 4MB
        val ExtRAMReverseOrder     = false
        val enableBranchPrediction = true
        val advancedDebug          = false
        val branchUpdateBypass     = false

        def debugSignal[T <: Data](signal: T): Unit = {
            if (DEBUG_SIGNALS) {
                dontTouch(signal)
            }
        }
    }

    object RAM_MAP {
        val BASE_RAM_START = 0x80000000L.U(32.W)
        val BASE_RAM_END   = 0x803fffffL.U(32.W)
        val EXT_RAM_START  = 0x80400000L.U(32.W)
        val EXT_RAM_END    = 0x807fffffL.U(32.W)
        val UART_DATA      = 0xbfd003f8L.U(32.W)
        val UART_FLAGS     = 0xbfd003fcL.U(32.W)
    }

    object ALU_OP {
        val SRL  = "b0000".U // 逻辑右移 Shift Right Logical
        val SLL  = "b0001".U // 逻辑左移 Shift Left Logical
        val SRA  = "b0010".U // 算术右移 Shift Right Arithmetic
        val OR   = "b0011".U // 逻辑或 Logical OR
        val AND  = "b0100".U // 逻辑与 Logical AND
        val XOR  = "b0101".U // 逻辑异或 Logical XOR
        val ADD  = "b0110".U // 加法 Add
        val SUB  = "b0111".U // 减法 Subtract
        val SLT  = "b1000".U // 小于 Set Less Than
        val SLTU = "b1001".U // 小于无符号 Set Less Than Unsigned
        val A    = "b1010".U // 直接传递 A
    }

    object BRANCH_INST {
        val NOT  = "b0000".U
        val BNE  = "b0001".U
        val BEQ  = "b0010".U
        val JIRL = "b0011".U
        val B    = "b0100".U
        val BL   = "b0101".U
        val BLT  = "b0110".U
        val BGE  = "b0111".U
        val BLTU = "b1000".U
        val BGEU = "b1001".U
    }

    object BRANCH_TYPE {
        val DIRECT    = "b00".U
        val CONDITION = "b01".U
        val CALL      = "b10".U
        val RETURN    = "b11".U
    }

    object MEM_SIZE {
        val BYTE     = "b00".U
        val HALFWORD = "b01".U
        val WORD     = "b11".U
    }

    object FU_TYPE {
        val ALU = "b00".U
        val MEM = "b10".U
        val MUL = "b11".U
    }

    object EXECUTION_CYCLE {
        val ALU = 1
        val MEM = 2
        val MUL = 2
    }
}
