// Designed by Kyle. 2025-03-12 15:27
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import CONSTANTS._
import CONSTANTS.CONFIG.debugSignal

/*
    指令解码器
    LoongArch 32位精简指令集（部分）
 */
class Decoder extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val PC          = Input(UInt(32.W))
            val PC_next     = Input(UInt(32.W)) // 预测的 PC
            val instruction = Input(UInt(32.W))
        }
        val out = new Bundle {
            val branch_inst = Output(UInt(4.W))  // 分支种类
            val uiL         = Output(UInt(32.W)) // 立即数1
            val uiR         = Output(UInt(32.W)) // 立即数2（仅供pcaddu12i）
            val srcL        = Output(UInt(5.W))  // 源操作数1
            val srcR        = Output(UInt(5.W))  // 源操作数2
            val valL        = Output(Bool())     // 源操作数1寄存器是否有效
            val valR        = Output(Bool())     // 源操作数2寄存器是否有效
            val dest        = Output(UInt(5.W))  // 目的操作数
            val FU          = Output(UInt(2.W))  // 使用哪个功能单元
            val ALU_OP      = Output(UInt(4.W))  // ALU操作码
            val mem_size    = Output(UInt(2.W))  // 访存大小
            val reg_write   = Output(Bool())     // 寄存器写使能
            val mem_write   = Output(Bool())     // 内存写使能
            val mem_read    = Output(Bool())
            val true_PC     = Output(UInt(32.W)) // 真实的PC
            val PCredirect  = Output(Bool())
            val reFetch     = Output(Bool())     // 分支预测失败，重新获取指令

            val unknown = Output(Bool()) // 未知指令
        }
    })

    val rk       = io.in.instruction(14, 10)
    val rj       = io.in.instruction(9, 5)
    val rd       = io.in.instruction(4, 0)
    val offset16 = io.in.instruction(25, 10)
    val offset26 = Cat(io.in.instruction(9, 0), io.in.instruction(25, 10))
    val si12     = io.in.instruction(21, 10)
    val si20     = io.in.instruction(24, 5)
    val si26     = io.in.instruction(25, 0)
    val ui5      = io.in.instruction(14, 10)

    // 默认值
    io.out.branch_inst := BRANCH_INST.NOT
    io.out.uiL         := 0.U
    io.out.uiR         := 0.U
    io.out.srcL        := 0.U
    io.out.srcR        := 0.U
    io.out.valL        := false.B
    io.out.valR        := false.B
    io.out.dest        := 0.U
    io.out.FU          := 0.U
    io.out.ALU_OP      := 0.U
    io.out.reg_write   := false.B
    io.out.mem_read    := false.B
    io.out.mem_write   := false.B
    io.out.mem_size    := MEM_SIZE.WORD
    io.out.true_PC     := 0.U
    io.out.reFetch     := false.B
    io.out.PCredirect  := false.B
    io.out.unknown     := false.B

    // 6位的指令码
    when(io.in.instruction(31, 26) === "b010111".U) { // bne 不相等跳转
        io.out.branch_inst := BRANCH_INST.BNE
        io.out.uiL    := io.in.PC + Cat(Fill(14, offset16(15)), offset16, "b00".U(2.W))
        io.out.srcL   := rj
        io.out.srcR   := rd
        io.out.valL   := true.B
        io.out.valR   := true.B
        io.out.ALU_OP := ALU_OP.SUB
    }.elsewhen(io.in.instruction(31, 26) === "b010110".U) { // beq 相等跳转
        io.out.branch_inst := BRANCH_INST.BEQ
        io.out.uiL    := io.in.PC + Cat(Fill(14, offset16(15)), offset16, "b00".U(2.W))
        io.out.srcL   := rj
        io.out.srcR   := rd
        io.out.valL   := true.B
        io.out.valR   := true.B
        io.out.ALU_OP := ALU_OP.SUB
    }.elsewhen(io.in.instruction(31, 26) === "b011000".U) { // blt 小于跳转
        io.out.branch_inst := BRANCH_INST.BLT
        io.out.uiL    := io.in.PC + Cat(Fill(14, offset16(15)), offset16, "b00".U(2.W))
        io.out.srcL   := rj
        io.out.srcR   := rd
        io.out.valL   := true.B
        io.out.valR   := true.B
        io.out.ALU_OP := ALU_OP.SLT
    }.elsewhen(io.in.instruction(31, 26) === "b011001".U) { // bge 大于等于跳转
        io.out.branch_inst := BRANCH_INST.BGE
        io.out.uiL    := io.in.PC + Cat(Fill(14, offset16(15)), offset16, "b00".U(2.W))
        io.out.srcL   := rj
        io.out.srcR   := rd
        io.out.valL   := true.B
        io.out.valR   := true.B
        io.out.ALU_OP := ALU_OP.SLT
    }.elsewhen(io.in.instruction(31, 26) === "b011010".U) { // bltu 小于无符号跳转
        io.out.branch_inst := BRANCH_INST.BLTU
        io.out.uiL    := io.in.PC + Cat(Fill(14, offset16(15)), offset16, "b00".U(2.W))
        io.out.srcL   := rj
        io.out.srcR   := rd
        io.out.valL   := true.B
        io.out.valR   := true.B
        io.out.ALU_OP := ALU_OP.SLTU
    }.elsewhen(io.in.instruction(31, 26) === "b011011".U) { // bgeu 大于等于无符号跳转
        io.out.branch_inst := BRANCH_INST.BGEU
        io.out.uiL    := io.in.PC + Cat(Fill(14, offset16(15)), offset16, "b00".U(2.W))
        io.out.srcL   := rj
        io.out.srcR   := rd
        io.out.valL   := true.B
        io.out.valR   := true.B
        io.out.ALU_OP := ALU_OP.SLTU
    }.elsewhen(io.in.instruction(31, 26) === "b010011".U) { // jirl 从寄存器跳转并链接
        // GR[rd] = PC + 4
        // PC = GR[rj] + SignExtend({offs16, 2'b00}, 32)
        // uiL = PC + 4, srcR = rj; uiR = SignExtend({offs16, 2'b00}, 32); ALU_OP: F = A;
        io.out.branch_inst := BRANCH_INST.JIRL
        io.out.uiL         := io.in.PC + 4.U
        io.out.uiR         := Cat(Fill(14, offset16(15)), offset16, "b00".U(2.W))
        io.out.srcR        := rj
        io.out.valR        := true.B
        io.out.dest        := rd
        io.out.reg_write   := true.B
        io.out.ALU_OP      := ALU_OP.A
    }.elsewhen(io.in.instruction(31, 26) === "b010100".U) { // b 无条件跳转 空操作：预解码阶段已处理
        io.out.branch_inst := BRANCH_INST.B
        val offset  = Cat(Fill(4, offset26(25)), offset26, "b00".U(2.W))
        val true_PC = io.in.PC.asSInt + offset.asSInt
        io.out.PCredirect := true.B
        debugSignal(offset)
        io.out.true_PC := true_PC.asUInt
        when(true_PC =/= io.in.PC_next.asSInt) {
            io.out.reFetch := true.B
        }
        io.out.uiL := true_PC.asUInt
    }.elsewhen(io.in.instruction(31, 26) === "b010101".U) { // bl 无条件跳转并链接
        io.out.branch_inst := BRANCH_INST.BL
        val offset  = Cat(Fill(4, offset26(25)), offset26, "b00".U(2.W))
        val true_PC = io.in.PC.asSInt + offset.asSInt
        io.out.PCredirect := true.B
        io.out.true_PC := true_PC.asUInt
        when(true_PC =/= io.in.PC_next.asSInt) {
            io.out.reFetch := true.B
        }
        io.out.uiL       := io.in.PC + 4.U
        io.out.dest      := 1.U
        io.out.reg_write := true.B
        io.out.ALU_OP    := ALU_OP.OR
    }.otherwise {
        // 7位的指令码
        when(io.in.instruction(31, 25) === "b0001010".U) { // lu12i.w 立即数左移12位
            io.out.uiL       := Cat(Fill(20, si20(19)), si20, 0.U(12.W))
            io.out.ALU_OP    := ALU_OP.OR
            io.out.dest      := rd
            io.out.reg_write := true.B
        }.elsewhen(io.in.instruction(31, 25) === "b0001110".U) { // pcaddu12i 保存偏移PC
            io.out.uiL       := io.in.PC
            io.out.ALU_OP    := ALU_OP.ADD
            io.out.uiR       := Cat(Fill(20, si20(19)), si20, 0.U(12.W))
            io.out.dest      := rd
            io.out.reg_write := true.B
        }.elsewhen(io.in.instruction(31, 25) === "b0000001".U) { // 立即数常规计算类：10位指令码
            io.out.srcR      := rj
            io.out.valR      := true.B
            io.out.dest      := rd
            io.out.reg_write := true.B
            switch(io.in.instruction(24, 22)) {
                is("b000".U) { // slti.w 立即数小于置位
                    io.out.uiL    := Cat(Fill(20, si12(11)), si12)
                    io.out.ALU_OP := ALU_OP.SLT
                }
                is("b001".U) { // slti.w 立即数小于无符号置位
                    io.out.uiL    := Cat(0.U(20.W), si12)
                    io.out.ALU_OP := ALU_OP.SLTU
                }
                is("b010".U) { // addi.w 立即数加法
                    io.out.uiL    := Cat(Fill(20, si12(11)), si12)
                    io.out.ALU_OP := ALU_OP.ADD
                }
                is("b101".U) { // andi 立即数与
                    io.out.uiL    := Cat(0.U(20.W), si12)
                    io.out.ALU_OP := ALU_OP.AND
                }
                is("b110".U) { // ori 立即数或
                    io.out.uiL    := Cat(0.U(20.W), si12)
                    io.out.ALU_OP := ALU_OP.OR
                }
                is("b111".U) { // xori 立即数异或
                    io.out.uiL    := Cat(0.U(20.W), si12)
                    io.out.ALU_OP := ALU_OP.XOR
                }
            }
        }.elsewhen(io.in.instruction(31, 25) === "b0010100".U) { // 访存类：10位指令码
            switch(io.in.instruction(24, 22)) {
                is("b110".U) { // 向内存中存字
                    io.out.FU        := FU_TYPE.MEM
                    io.out.uiL       := Cat(Fill(20, si12(11)), si12)
                    io.out.srcL      := rj
                    io.out.valL      := true.B
                    io.out.srcR      := rd
                    io.out.valR      := true.B
                    io.out.mem_write := true.B
                    io.out.mem_size  := MEM_SIZE.WORD
                }
                is("b010".U) { // 从内存中取字
                    io.out.FU        := FU_TYPE.MEM
                    io.out.uiL       := Cat(Fill(20, si12(11)), si12)
                    io.out.srcL      := rj
                    io.out.valL      := true.B
                    io.out.dest      := rd
                    io.out.reg_write := true.B
                    io.out.mem_read  := true.B
                    io.out.mem_size  := MEM_SIZE.WORD
                }
                is("b100".U) { // 向内存中存字节
                    io.out.FU        := FU_TYPE.MEM
                    io.out.uiL       := Cat(Fill(20, si12(11)), si12)
                    io.out.srcL      := rj
                    io.out.valL      := true.B
                    io.out.srcR      := rd
                    io.out.valR      := true.B
                    io.out.mem_write := true.B
                    io.out.mem_size  := MEM_SIZE.BYTE
                }
                is("b000".U) { // 从内存中取字节
                    io.out.FU        := FU_TYPE.MEM
                    io.out.uiL       := Cat(Fill(20, si12(11)), si12)
                    io.out.srcL      := rj
                    io.out.valL      := true.B
                    io.out.dest      := rd
                    io.out.reg_write := true.B
                    io.out.mem_read  := true.B
                    io.out.mem_size  := MEM_SIZE.BYTE
                }
            }
        }.elsewhen(io.in.instruction(31, 25) === "b0000000".U) { // 寄存器间计算类：17位指令码
            io.out.dest      := rd
            io.out.reg_write := true.B
            when(io.in.instruction(24, 15) === "b0000100000".U) { // add.w 加法
                io.out.ALU_OP := ALU_OP.ADD
                io.out.srcL   := rk
                io.out.valL   := true.B
                io.out.srcR   := rj
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0000101010".U) { // or 或运算
                io.out.ALU_OP := ALU_OP.OR
                io.out.srcL   := rj
                io.out.valL   := true.B
                io.out.srcR   := rk
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0000101011".U) { // xor 异或运算
                io.out.ALU_OP := ALU_OP.XOR
                io.out.srcL   := rj
                io.out.valL   := true.B
                io.out.srcR   := rk
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0000100010".U) { // sub.w 减法
                io.out.ALU_OP := ALU_OP.SUB
                io.out.srcL   := rj
                io.out.valL   := true.B
                io.out.srcR   := rk
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0000101001".U) { // and 与运算
                io.out.ALU_OP := ALU_OP.AND
                io.out.srcL   := rj
                io.out.valL   := true.B
                io.out.srcR   := rk
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0000101110".U) { // sll.w 逻辑左移
                io.out.ALU_OP := ALU_OP.SLL
                io.out.srcL   := rj
                io.out.valL   := true.B
                io.out.srcR   := rk
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0000101111".U) { // srl.w 逻辑右移
                io.out.ALU_OP := ALU_OP.SRL
                io.out.srcL   := rj
                io.out.valL   := true.B
                io.out.srcR   := rk
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0000110000".U) {
                io.out.ALU_OP := ALU_OP.SRA // 算术右移
                io.out.srcL   := rj
                io.out.valL   := true.B
                io.out.srcR   := rk
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0010001001".U) { // srli.w 逻辑右移
                io.out.ALU_OP := ALU_OP.SRL
                io.out.uiL    := ui5
                io.out.srcR   := rj
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0010010001".U) { // srai.w 算术右移
                io.out.ALU_OP := ALU_OP.SRA
                io.out.uiL    := ui5
                io.out.srcR   := rj
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0010000001".U) { // slli.w 逻辑左移
                io.out.ALU_OP := ALU_OP.SLL
                io.out.uiL    := ui5
                io.out.srcR   := rj
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0000100100".U) { // slt 小于置位
                io.out.ALU_OP := ALU_OP.SLT
                io.out.srcL   := rj
                io.out.valL   := true.B
                io.out.srcR   := rk
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0000100101".U) { // sltu 小于无符号置位
                io.out.ALU_OP := ALU_OP.SLTU
                io.out.srcL   := rj
                io.out.valL   := true.B
                io.out.srcR   := rk
                io.out.valR   := true.B
            }.elsewhen(io.in.instruction(24, 15) === "b0000111000".U) { // mul.w 乘法
                io.out.FU   := FU_TYPE.MUL
                io.out.srcL := rk
                io.out.valL := true.B
                io.out.srcR := rj
                io.out.valR := true.B
            }.otherwise {
                io.out.unknown := true.B
            }
        }.otherwise {
            // 其他指令：未知指令
            io.out.unknown := true.B
        }

        when(io.in.instruction === "h03400000".U) {
            io.out.unknown := true.B // nop 指令，置unknown有效以使valid信号无效
        }
    }
}

class DecoderArray extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val PC               = Input(UInt(32.W))
            val PC_next          = Input(UInt(32.W))     // 预测的 PC
            val instruction      = Input(Vec(4, UInt(32.W)))
            val offset_predicted = Input(UInt(2.W))      // 分支预测器的偏移预测
            val valid            = Input(Vec(4, Bool())) // 有效掩码——来自F/D
            val pause            = Input(Bool())
            val predict_taken    = Input(Bool())         // 分支预测器的预测结果
        }
        val out = new Bundle {
            val branch_inst = Output(Vec(4, UInt(4.W)))  // 分支种类
            val uiL         = Output(Vec(4, UInt(32.W))) // 立即数1
            val uiR         = Output(Vec(4, UInt(32.W))) // 立即数2（仅供pcaddu12i）
            val srcL        = Output(Vec(4, UInt(5.W)))  // 源操作数1
            val srcR        = Output(Vec(4, UInt(5.W)))  // 源操作数2
            val valL        = Output(Vec(4, Bool()))     // 源操作数1寄存器是否有效
            val valR        = Output(Vec(4, Bool()))     // 源操作数2寄存器是否有效
            val dest        = Output(Vec(4, UInt(5.W)))  // 目的操作数
            val FU          = Output(Vec(4, UInt(2.W)))  // 使用哪个功能单元
            val ALU_OP      = Output(Vec(4, UInt(4.W)))  // ALU操作码
            val mem_size    = Output(Vec(4, UInt(2.W)))  // 访存大小
            val reg_write   = Output(Vec(4, Bool()))     // 寄存器写使能
            val mem_write   = Output(Vec(4, Bool()))     // 内存写使能
            val mem_read    = Output(Vec(4, Bool()))     // 内存读使能
            val true_PC     = Output(UInt(32.W))         // 真实的PC

            val reFetch          = Output(Bool())         // 分支预测失败，重新获取指令
            val PCredirect       = Output(Bool())
            val valid            = Output(Vec(4, Bool())) // 有效掩码
            val offset_predicted = Output(UInt(2.W))      // 分支预测器的偏移预测
            val predict_taken    = Output(Bool())         // 分支预测器的预测结果

            val PC_next = Output(UInt(32.W)) // 预测的 PC - 如果出现更新,则需要修正
        }
    })
    val decoder = Seq.fill(4)(Module(new Decoder()))

    io.out.PCredirect := !io.in.pause && (0 until 4)
        .map(i => decoder(i).io.out.PCredirect & io.in.valid(i))
        .reduce(_ || _)

    val valid_if_branch = VecInit(Seq.fill(4)(false.B))
    valid_if_branch(0) := true.B

    for (i <- 0 until 3) {
        valid_if_branch(i + 1) := valid_if_branch(i) &&
            !(decoder(i).io.out.PCredirect && io.in.valid(i))
    }

    val valid_if_mask = VecInit(Seq.fill(4)(false.B))

    val offset_predicted_valid_wire = Wire(Bool())
    offset_predicted_valid_wire := io.in.valid(io.in.offset_predicted) && VecInit(
      decoder.map(_.io.out.branch_inst)
    )(io.in.offset_predicted) =/= BRANCH_INST.NOT
    debugSignal(offset_predicted_valid_wire)

    valid_if_mask(0) := valid_if_branch(0)
    for (i <- 1 until 4) {
        valid_if_mask(i) := valid_if_branch(i) &&
            (
              ((i.U <= io.in.offset_predicted) || !offset_predicted_valid_wire) ||
                  !io.in.predict_taken
            )
    }

    debugSignal(valid_if_branch)
    debugSignal(valid_if_mask)

    for (i <- 0 until 4) {
        io.out.valid(i) := io.in.valid(i) &&
            valid_if_mask(i) &&
            !decoder(i).io.out.unknown
    }
    val reFetch_for_branch = !io.in.pause && (0 until 4)
        .map(i => decoder(i).io.out.reFetch & io.out.valid(i))
        .reduce(_ || _)

    // Convert UInt to Int for indexing by using .litValue.toInt, only safe if offset_predicted is a literal
    val decoder_branch_inst_vec = VecInit(decoder.map(_.io.out.branch_inst))
    val decoder_valid_vec       = VecInit(decoder.map(_.io.out.unknown === false.B))
    val decoder_unknown_vec     = VecInit(decoder.map(_.io.out.unknown))

    val reFetch_for_no_branch =
        io.in.valid(io.in.offset_predicted) &&
            io.in.predict_taken === true.B &&
            !decoder_unknown_vec(io.in.offset_predicted) &&
            decoder_valid_vec(io.in.offset_predicted) &&
            decoder_branch_inst_vec(io.in.offset_predicted) === BRANCH_INST.NOT

    debugSignal(reFetch_for_branch)
    debugSignal(reFetch_for_no_branch)

    io.out.reFetch := reFetch_for_branch || reFetch_for_no_branch

    io.out.true_PC := 0.U(32.W)
    // when(reFetch_for_branch) {
    //     // 如果有多个指令同时设置PC，则取最后早一个设置的PC
    //     for (i <- 3 until -1 by -1) {
    //         when(decoder(i).io.out.reFetch && io.in.valid(i)) {
    //             io.out.true_PC := decoder(i).io.out.true_PC
    //         }
    //     }
    // }.elsewhen(reFetch_for_no_branch) {
    //     io.out.true_PC := Cat(io.in.PC(31, 4) + 1.U, 0.U(4.W))
    // }
    when(reFetch_for_branch || reFetch_for_no_branch) {
        io.out.true_PC := Cat(io.in.PC(31, 4) + 1.U, 0.U(4.W))
        // 如果有多个指令同时设置PC，则取最后早一个设置的PC
        for (i <- 3 until -1 by -1) {
            when(decoder(i).io.out.PCredirect && io.in.valid(i)) {
                io.out.true_PC := decoder(i).io.out.true_PC
            }
        }
    }

    io.out.PC_next := Mux(io.out.reFetch, io.out.true_PC, io.in.PC_next)
    io.out.offset_predicted := Mux(
      reFetch_for_no_branch,
      Mux(
        (0 until 4).map(i => decoder(i).io.out.PCredirect).reduce(_ || _),
        PriorityEncoder((0 until 4).map(i => decoder(i).io.out.PCredirect)),
        3.U
      ),
      io.in.offset_predicted
    )
    io.out.predict_taken := Mux(reFetch_for_no_branch, false.B, io.in.predict_taken)

    for (i <- 0 until 4) {
        decoder(i).io.in.PC          := Cat(io.in.PC(31, 4), 0.U(4.W)) + (i.U << 2)
        decoder(i).io.in.PC_next     := io.in.PC_next
        decoder(i).io.in.instruction := io.in.instruction(i)
        io.out.branch_inst(i)        := decoder(i).io.out.branch_inst
        io.out.uiL(i)                := decoder(i).io.out.uiL
        io.out.uiR(i)                := decoder(i).io.out.uiR
        io.out.srcL(i)               := decoder(i).io.out.srcL
        io.out.srcR(i)               := decoder(i).io.out.srcR
        io.out.valL(i)               := decoder(i).io.out.valL
        io.out.valR(i)               := decoder(i).io.out.valR
        io.out.dest(i)               := decoder(i).io.out.dest
        io.out.FU(i)                 := decoder(i).io.out.FU
        io.out.ALU_OP(i)             := decoder(i).io.out.ALU_OP
        io.out.reg_write(i)          := decoder(i).io.out.reg_write
        io.out.mem_write(i)          := decoder(i).io.out.mem_write
        io.out.mem_read(i)           := decoder(i).io.out.mem_read
        io.out.mem_size(i)           := decoder(i).io.out.mem_size
    }
}

object Decoder extends App {
    ChiselStage.emitSystemVerilogFile(
      new DecoderArray(),
      Array("--target-dir", "generated")
    )
}
