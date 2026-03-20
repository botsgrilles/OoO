// Designed by Kyle. 2025-03-12 19:33
import chisel3._
import circt.stage.ChiselStage
import CONSTANTS._
import chisel3.util.switch
import chisel3.util.is

/*
    基本算术逻辑单元
 */
class ALU extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val inA    = Input(UInt(32.W))
            val inB    = Input(UInt(32.W))
            val ALU_OP = Input(UInt(4.W))
        }
        val out = new Bundle {
            val OF     = Output(Bool())     // 溢出标志
            val ZF     = Output(Bool())     // 零标志
            val SF     = Output(Bool())     // 符号标志
            val CF     = Output(Bool())     // 进位标志
            val result = Output(UInt(32.W)) // ALU输出
        }
    })

    // 默认值
    io.out.result := 0.U
    val OF = RegInit(false.B)
    val CF = RegInit(false.B)
    io.out.OF     := false.B // 溢出标志
    io.out.CF     := false.B // 进位标志

    val result = RegInit(0.U(32.W)) // 寄存器用于存储结果

    switch(io.in.ALU_OP) {
        is(ALU_OP.SRL) { // 逻辑右移
            result := io.in.inB >> io.in.inA(4, 0)
        }
        is(ALU_OP.SLL) { // 逻辑左移
            result := io.in.inB << io.in.inA(4, 0)
        }
        is(ALU_OP.SRA) {
            result := (io.in.inB.asSInt >> io.in.inA(4, 0)).asUInt // 算术右移
        }
        is(ALU_OP.OR) { // 逻辑或
            result := io.in.inA | io.in.inB
        }
        is(ALU_OP.AND) { // 逻辑与
            result := io.in.inA & io.in.inB
        }
        is(ALU_OP.XOR) { // 逻辑异或
            result := io.in.inA ^ io.in.inB
            // Now mapped to EQUAL
            // result := Mux(io.in.inA === io.in.inB, 1.U, 0.U)
        }
        is(ALU_OP.ADD) { // 加法
            result := io.in.inA + io.in.inB
            CF     := (io.in.inA + io.in.inB) < io.in.inA // 进位标志
            OF := (io.in.inA(31) === io.in.inB(31)) && (result(31) =/= io.in
                .inA(31)) // 溢出标志
        }
        is(ALU_OP.SUB) { // 减法
            result := io.in.inA - io.in.inB
            CF     := (io.in.inA - io.in.inB) > io.in.inA // 进位标志
            OF := (io.in.inA(31) =/= io.in.inB(31)) && (result(31) =/= io.in
                .inA(31)) // 溢出标志
        }
        is(ALU_OP.SLT) { // 小于置位
            result := Mux(io.in.inA.asSInt < io.in.inB.asSInt, 1.U, 0.U)
        }
        is(ALU_OP.SLTU) { // 小于无符号置位
            result := Mux(io.in.inA < io.in.inB, 1.U, 0.U)
        }
        is(ALU_OP.A) {
            result := io.in.inA // 直接输出A
        }
    }

    io.out.SF := result(31)       // 符号标志
    io.out.ZF := (result === 0.U) // 零标志
    io.out.CF := CF
    io.out.OF := OF
    io.out.result := result // 输出结果
}

object ALU extends App {
    ChiselStage.emitSystemVerilogFile(new ALU(), Array("--target-dir", "generated"))
}
