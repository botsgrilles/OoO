// Designed by Kyle. 2025-03-12 20:16
import chisel3._
import circt.stage.ChiselStage

class Pipline_D_RN(combinational: true) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val PC            = Input(UInt(32.W))
            val PC_next       = Input(UInt(32.W))
            val PC_decode     = Input(UInt(32.W))
            val decode_PC_set = Input(Bool())
            val predicted_taken = Input(Bool()) // 分支预测器的预测结果
            val offset_predicted = Input(UInt(2.W))
            val pause         = Input(Bool())
            val clear         = Input(Bool())
            val decoded = Vec(
              4,
              new Bundle {
                  val branch_inst = Input(UInt(4.W))  // 分支种类
                  val uiL         = Input(UInt(32.W)) // 立即数1
                  val uiR         = Input(UInt(32.W)) // 立即数2（仅供pcaddu12i）
                  val srcL        = Input(UInt(5.W))  // 源操作数1
                  val srcR        = Input(UInt(5.W))  // 源操作数2
                  val valL        = Input(Bool())     // 源操作数1寄存器是否有效
                  val valR        = Input(Bool())     // 源操作数2寄存器是否有效
                  val dest        = Input(UInt(5.W))  // 目的操作数
                  val FU          = Input(UInt(2.W))  // 使用哪个功能单元
                  val ALU_OP      = Input(UInt(4.W))  // ALU操作码
                  val mem_size    = Input(UInt(2.W))  // 访存大小
                  val reg_write   = Input(Bool())     // 寄存器写使能
                  val mem_write   = Input(Bool())     // 内存写使能
                  val mem_read    = Input(Bool())     // 内存读使能
              }
            )
            val valid_F_D     = Input(Vec(4, Bool())) // 有效信号 来自 F_D
            val valid_Decoder = Input(Vec(4, Bool())) // 有效信号 来自 Decoder
        }
        val out = new Bundle {
            val PC      = Output(UInt(32.W))
            val PC_next = Output(UInt(32.W))
            val PC_decode     = Output(UInt(32.W))
            val offset_predicted = Output(UInt(2.W))
            val predicted_taken = Output(Bool()) // 分支预测器的预测结果
            val decode_PC_set = Output(Bool())
            val decoded = Vec(
              4,
              new Bundle {
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
                  val mem_read    = Output(Bool())     // 内存读使能
              }
            )
            val valid         = Output(Vec(4, Bool())) // 寄存器是否有效
            val valid_delayed = Output(Vec(4, Bool())) // 寄存器是否有效
        }
    })

    // Create a default decoded Bundle with all fields explicitly initialized
    val defaultDecoded = Wire(new Bundle {
        val branch_inst = UInt(4.W)
        val uiL         = UInt(32.W)
        val uiR         = UInt(32.W)
        val srcL        = UInt(5.W)
        val srcR        = UInt(5.W)
        val valL        = Bool()
        val valR        = Bool()
        val dest        = UInt(5.W)
        val FU          = UInt(2.W)
        val ALU_OP      = UInt(4.W)
        val mem_size    = UInt(2.W)
        val reg_write   = Bool()
        val mem_write   = Bool()
        val mem_read    = Bool()
    })

    defaultDecoded.branch_inst := 0.U
    defaultDecoded.uiL         := 0.U
    defaultDecoded.uiR         := 0.U
    defaultDecoded.srcL        := 0.U
    defaultDecoded.srcR        := 0.U
    defaultDecoded.valL        := false.B
    defaultDecoded.valR        := false.B
    defaultDecoded.dest        := 0.U
    defaultDecoded.FU          := 0.U
    defaultDecoded.ALU_OP      := 0.U
    defaultDecoded.mem_size    := 0.U
    defaultDecoded.reg_write   := false.B
    defaultDecoded.mem_write   := false.B
    defaultDecoded.mem_read    := false.B

    val PC      = RegInit(0.U(32.W))
    val PC_next = RegInit(0.U(32.W))
    val PC_decode = RegInit(0.U(32.W))
    val decode_PC_set = RegInit(false.B)
    val offset_predicted = RegInit(0.U(2.W))
    val predicted_taken = RegInit(false.B)
    val decoded = RegInit(VecInit(Seq.fill(4)(defaultDecoded)))
    val valid   = RegInit(VecInit(Seq.fill(4)(false.B))) // 寄存器是否有效

    // F/D 和 Decoder 的有效信号均有效时才最终有效
    for (i <- 0 until 4) {
        valid(i) := io.in.valid_Decoder(i)
    }

    when(io.in.clear || reset.asBool) {
        PC      := 0.U
        PC_next := 0.U
        PC_decode := 0.U
        decode_PC_set := false.B
        offset_predicted := 0.U
        predicted_taken := false.B
        decoded := VecInit(Seq.fill(4)(defaultDecoded))
        valid   := VecInit(Seq.fill(4)(false.B))
    }.elsewhen(io.in.pause) {
        PC      := PC
        PC_next := PC_next
        PC_decode := PC_decode
        decode_PC_set := decode_PC_set
        offset_predicted := offset_predicted
        predicted_taken := predicted_taken
        decoded := decoded
        valid   := valid
        // valid   := VecInit(Seq.fill(4)(false.B))
    }.otherwise {
        PC      := io.in.PC
        PC_next := io.in.PC_next
        PC_decode := io.in.PC_decode
        decode_PC_set := io.in.decode_PC_set
        offset_predicted := io.in.offset_predicted
        predicted_taken := io.in.predicted_taken
        decoded := io.in.decoded
    }

    // Create delayed registers for PC, PC_next, and valid
    if (combinational) {
        io.out.PC            := PC
        io.out.PC_next       := PC_next
        io.out.valid_delayed := valid
        io.out.PC_decode     := PC_decode
        io.out.decode_PC_set := decode_PC_set
        io.out.offset_predicted := offset_predicted
        io.out.predicted_taken := predicted_taken
    } else {
        val delayed_PC      = RegNext(PC)
        val delayed_PC_next = RegNext(PC_next)
        val delayed_PC_decode = RegNext(PC_decode)
        val delayed_decode_PC_set = RegNext(io.in.decode_PC_set)
        val delayed_offset_predicted = RegNext(offset_predicted)
        val delayed_predicted_taken = RegNext(predicted_taken)
        val delayed_valid   = RegNext(valid)

        io.out.PC            := delayed_PC
        io.out.PC_next       := delayed_PC_next
        io.out.PC_decode     := delayed_PC_decode
        io.out.decode_PC_set := delayed_decode_PC_set
        io.out.offset_predicted := delayed_offset_predicted
        io.out.predicted_taken := delayed_predicted_taken
        io.out.valid_delayed := delayed_valid
    }

    io.out.valid := valid
    // For decoded outputs
    for (i <- 0 until 4) {
        // Set delayed fields
        if (combinational) {
            io.out.decoded(i).branch_inst := decoded(i).branch_inst
            io.out.decoded(i).uiL         := decoded(i).uiL
            io.out.decoded(i).uiR         := decoded(i).uiR
            io.out.decoded(i).valL        := decoded(i).valL
            io.out.decoded(i).valR        := decoded(i).valR
            io.out.decoded(i).FU          := decoded(i).FU
            io.out.decoded(i).ALU_OP      := decoded(i).ALU_OP
            io.out.decoded(i).mem_size    := decoded(i).mem_size
            io.out.decoded(i).reg_write   := decoded(i).reg_write
            io.out.decoded(i).mem_write   := decoded(i).mem_write
            io.out.decoded(i).mem_read    := decoded(i).mem_read
        } else {
            io.out.decoded(i).branch_inst := RegNext(decoded(i).branch_inst)
            io.out.decoded(i).uiL         := RegNext(decoded(i).uiL)
            io.out.decoded(i).uiR         := RegNext(decoded(i).uiR)
            io.out.decoded(i).valL        := RegNext(decoded(i).valL)
            io.out.decoded(i).valR        := RegNext(decoded(i).valR)
            io.out.decoded(i).FU          := RegNext(decoded(i).FU)
            io.out.decoded(i).ALU_OP      := RegNext(decoded(i).ALU_OP)
            io.out.decoded(i).mem_size    := RegNext(decoded(i).mem_size)
            io.out.decoded(i).reg_write   := RegNext(decoded(i).reg_write)
            io.out.decoded(i).mem_write   := RegNext(decoded(i).mem_write)
            io.out.decoded(i).mem_read    := RegNext(decoded(i).mem_read)
        }
        // Set non-delayed fields
        io.out.decoded(i).srcL := decoded(i).srcL
        io.out.decoded(i).srcR := decoded(i).srcR
        io.out.decoded(i).dest := decoded(i).dest
    }
}

object Pipline_D_RN extends App {
    ChiselStage.emitSystemVerilogFile(
      new Pipline_D_RN(combinational = true),
      Array("--target-dir", "generated")
    )
}
