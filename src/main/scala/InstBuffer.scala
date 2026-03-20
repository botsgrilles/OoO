import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

/*
    Inst Buffer——取指令缓存
    根据当前PC，最多写入写入4条指令；一周期内最多取出4条指令
 */

class InstBuffer(capacity: Int) extends Module {
    val io = new Bundle {
        val in = new Bundle {
            val clear = Input(Bool())             // 清除指令缓存
            val pause = Input(Bool())             // 暂停输出
            val insts = Input(Vec(4, UInt(32.W))) // 输入的指令
            val PC    = Input(UInt(32.W))         // 当前PC
        }
        val out = new Bundle {
            val insts = Output(Vec(4, UInt(32.W))) // 输出的指令
            val PC    = Output(UInt(32.W))         // 当前PC
            val valid = Output(Vec(4, Bool()))     // 指令是否有效
        }
    }

    val buffer = RegInit(VecInit(Seq.fill(capacity)(WireInit(0.U.asTypeOf(new Bundle {
        val PC    = UInt(32.W)
        val inst  = UInt(32.W)
        val valid = Bool()
    })))))

    val head = RegInit(0.U(log2Ceil(capacity).W))
    val tail = RegInit(0.U(log2Ceil(capacity).W))
    val count = RegInit(0.U(log2Ceil(capacity + 1).W))

    // 清除缓存
    when(io.in.clear) {
        head := 0.U
        tail := 0.U
        count := 0.U
        for (i <- 0 until capacity) {
            buffer(i).valid := false.B
        }
    }

    // 写入指令
    val canWrite = (count <= (capacity - 4).U)
    when(!io.in.clear && canWrite) {
        for (i <- 0 until 4) {
            buffer((tail + i.U) % capacity.U).PC := io.in.PC + (i.U << 2)
            buffer((tail + i.U) % capacity.U).inst := io.in.insts(i)
            buffer((tail + i.U) % capacity.U).valid := true.B
        }
        tail := (tail + 4.U) % capacity.U
        count := count + 4.U
    }

    // 输出指令
    val outInsts = Wire(Vec(4, UInt(32.W)))
    val outPCs   = Wire(Vec(4, UInt(32.W)))
    val outValid = Wire(Vec(4, Bool()))
    for (i <- 0 until 4) {
        val idx = (head + i.U) % capacity.U
        outInsts(i) := buffer(idx).inst
        outPCs(i)   := buffer(idx).PC
        outValid(i) := buffer(idx).valid && (i.U < count)
    }

    // 只有在pause为false时才出队
    when(!io.in.pause && !io.in.clear) {
        val deqCount = Mux(count >= 4.U, 4.U, count)
        for (i <- 0 until 4) {
            when(i.U < deqCount) {
                buffer((head + i.U) % capacity.U).valid := false.B
            }
        }
        head := (head + deqCount) % capacity.U
        count := count - deqCount
    }

    io.out.insts := outInsts
    io.out.PC := outPCs(0)
    io.out.valid := outValid
}
