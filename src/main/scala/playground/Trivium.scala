package playground

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class Trivium extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val key       = Input(UInt(80.W))
            val iv        = Input(UInt(80.W))
            val plaintext = Input(UInt(80.W))
            val start     = Input(Bool())
        }
        val out = new Bundle {
            val ciphertext = Output(UInt(80.W))
            val valid      = Output(Bool())
        }
    })

    val regA = RegInit(0.U(93.W))
    val regB = RegInit(0.U(84.W))
    val regC = RegInit(0.U(111.W))

    val ciphertext_reg = RegInit(0.U(80.W))

    // State machine definition
    val sIdle :: sLoad :: sWarmup :: sEncrypt :: sDone :: Nil = Enum(5)
    val state                                                 = RegInit(sIdle)

    // Latch inputs in sIdle to ensure stable values during sLoad
    val latched_key       = Reg(UInt(80.W))
    val latched_iv        = Reg(UInt(80.W))
    val latched_plaintext = Reg(UInt(80.W))

    val warmupCounter  = RegInit(0.U(11.W)) // Need 11 bits for 1152 (0 to 1151)
    val encryptCounter = RegInit(0.U(7.W))  // Need 7 bits for 80 (0 to 79)

    io.out.ciphertext := ciphertext_reg
    io.out.valid      := false.B // Default to false

    val t1 =
        regA(92 - 65) ^ (regA(92 - 90) & regA(92 - 91)) ^ regA(92 - 92) ^ regB(83 - 77)
    val t2 =
        regB(83 - 68) ^ (regB(83 - 81) & regB(83 - 82)) ^ regB(83 - 83) ^ regC(110 - 86)
    val t3 =
        regC(110 - 65) ^ (regC(110 - 108) & regC(110 - 109)) ^ regC(110 - 110) ^ regA(
          92 - 68
        )

    val z = regA(92 - 65) ^ regA(92 - 92) ^ regB(83 - 68) ^ regB(83 - 83) ^ regC(
      110 - 65
    ) ^ regC(110 - 110)

    // --- State Machine Logic ---
    switch(state) {
        is(sIdle) {
            when(io.in.start) {
                // Latch inputs
                latched_key       := io.in.key
                latched_iv        := io.in.iv
                latched_plaintext := io.in.plaintext
                // Reset output register
                ciphertext_reg := 0.U
                state          := sLoad
            }
        }

        is(sLoad) {
            val reversed_key = Reverse(latched_key)
            val reversed_iv  = Reverse(latched_iv)

            regA := Cat(latched_key, 0.U((93 - 80).W)) // A[92:80]=0, A[79:0]=reversed_key
            regB := Cat(latched_iv, 0.U((84 - 80).W))  // B[83:80]=0, B[79:0]=reversed_iv
            regC := Cat(0.U(108.W), "b111".U(3.W))     // C[110:3]=0, C[2:0]=111

            // Reset counters
            warmupCounter  := 0.U
            encryptCounter := 0.U

            state := sWarmup
        }

        is(sWarmup) {
            regA := Cat(t3, regA(92, 1))
            regB := Cat(t1, regB(83, 1))
            regC := Cat(t2, regC(110, 1))

            warmupCounter := warmupCounter + 1.U

            // Check if warmup finished (1152 cycles completed)
            when(warmupCounter === 1151.U) {
                state := sEncrypt
                // encryptCounter is already 0 from sLoad
            }
        }

        is(sEncrypt) {

            regA := Cat(t3, regA(92, 1))
            regB := Cat(t1, regB(83, 1))
            regC := Cat(t2, regC(110, 1))

            val currentPlaintextBit  = latched_plaintext(encryptCounter)
            val currentCiphertextBit = z ^ currentPlaintextBit

            ciphertext_reg := ciphertext_reg.bitSet(encryptCounter, currentCiphertextBit)

            encryptCounter := encryptCounter + 1.U

            // Check if encryption finished (80 cycles completed)
            when(encryptCounter === 79.U) {
                state := sDone
            }
        }

        is(sDone) {
            // Output is valid in this state
            io.out.valid := true.B
            state        := sIdle
        }
    }
}

object Trivium extends App {
    println("Generating Verilog for Trivium...")
    ChiselStage.emitSystemVerilogFile(
      new Trivium(),
      Array("--target-dir", "generated"),
      firtoolOpts = Array(
        "-disable-all-randomization",
        "-strip-debug-info"
      ) // Optional: cleaner Verilog
    )
    println("Verilog generation complete in 'generated' directory.")
}
