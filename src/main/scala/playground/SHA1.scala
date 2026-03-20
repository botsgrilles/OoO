import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class Sha1IO extends Bundle {
    val start   = Input(Bool())
    val data_in = Input(UInt(512.W)) // 64字节块
    val valid   = Input(Bool())
    val digest  = Output(UInt(160.W))
    val done    = Output(Bool())
}

class Sha1 extends Module {
    val io = IO(new Sha1IO)

    val h0_reg = RegInit("h67452301".U(32.W)) 
    val h1_reg = RegInit("hefcdab89".U(32.W))
    val h2_reg = RegInit("h98badcfe".U(32.W))
    val h3_reg = RegInit("h10325476".U(32.W))
    val h4_reg = RegInit("hc3d2e1f0".U(32.W))

    val w     = Reg(Vec(80, UInt(32.W)))
    val round = RegInit(0.U(7.W))

    val a = Reg(UInt(32.W))
    val b = Reg(UInt(32.W))
    val c = Reg(UInt(32.W))
    val d = Reg(UInt(32.W))
    val e = Reg(UInt(32.W))

    val sIdle :: sBusy :: sInitW :: sRounds :: sUpdateH :: sOutput :: Nil = Enum(6)
    val state = RegInit(sIdle)

    io.done   := false.B
    io.digest := Cat(h0_reg, h1_reg, h2_reg, h3_reg, h4_reg) // Always output current H values

    switch(state) {
        is(sIdle) {
            when(io.start && io.valid) {
                // 解析输入块为16个32位字
                for (i <- 0 until 16) {
                    w(i) := io.data_in(511 - i * 32, 480 - i * 32)
                }
                // Initialize w(16-79) to 0. They will be overwritten.
                for (i <- 16 until 80) {
                    w(i) := 0.U 
                }
                a     := h0_reg; b := h1_reg; c := h2_reg; d := h3_reg; e := h4_reg
                round := 0.U
                state := sBusy // Changed from sInit as sInit logic was non-functional
            }
        }
        // sBusy state ensures inputs are sampled and then we proceed
        is(sBusy) {
             state := sRounds // Directly to rounds, sInit was a NOP.
        }
        // is(sInitW) { ... } // This state is removed as it was a NOP.

        is(sRounds) {
            val f = Wire(UInt(32.W))
            val k = Wire(UInt(32.W))
            // Default to avoid latches if a condition isn't met (though here all paths are covered)
            f := 0.U
            k := 0.U 

            when(round < 20.U) {
                f := (b & c) | (~b & d)
                k := "h5a827999".U
            }.elsewhen(round < 40.U) {
                f := b ^ c ^ d
                k := "h6ed9eba1".U
            }.elsewhen(round < 60.U) {
                f := (b & c) | (b & d) | (c & d)
                k := "h8f1bbcdc".U
            }.otherwise { // round < 80
                f := b ^ c ^ d
                k := "hca62c1d6".U
            }
            val temp = Wire(UInt(32.W))
            temp := a.rotateLeft(5) + f + e + k + w(round)

            e := d
            d := c
            c := b.rotateLeft(30)
            b := a
            a := temp

            when(round === 79.U) {
                state := sUpdateH // Go to update H registers
            }.otherwise {
                round := round + 1.U
                // Pipelined W-expansion: Calculate W(t+1) in round t
                // This is for W(16) through W(79)
                when(round >= 15.U) { // Start calculating W(16) when round is 15
                    w(round + 1.U) := (w(round - 2.U) ^ w(round - 7.U) ^ w(
                      round - 13.U // W(t-14) where t = round+1
                    ) ^ w(round - 15.U)).rotateLeft(1) // W(t-16) where t = round+1
                }
            }
        }
        is(sUpdateH) { // New state: H registers are updated on the next clock edge
            h0_reg := h0_reg + a
            h1_reg := h1_reg + b
            h2_reg := h2_reg + c
            h3_reg := h3_reg + d
            h4_reg := h4_reg + e
            state  := sOutput // Transition to output state
        }
        is(sOutput) { // New state: done is asserted, digest now reflects updated H values
            io.done := true.B
            state   := sIdle
        }
    }
}

object Sha1Main extends App {
    ChiselStage.emitSystemVerilogFile(new Sha1, Array("--target-dir", "generated"))
}