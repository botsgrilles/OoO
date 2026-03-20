// Designed by Kyle. 2025-04-10 14:17
import chisel3._
import circt.stage.ChiselStage
import chisel3.util.Cat

class InstRAMInterface extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val PC    = Input(UInt(32.W)) // 地址
            val mask  = Input(Bool())
        }
        val out = new Bundle {
            val Insts             = Output(Vec(4, UInt(32.W))) // 指令数据
            val addr              = Output(UInt(32.W))         // 地址
            val inst_valid        = Output(Bool())             // 是否有效
            val req_valid         = Output(Bool())             // 请求有效
            val cacheStallRequest = Output(Bool())             // Cache是否stall
        }
        // ICache接口
        val icache = Flipped(new ICacheInterface().cpu)
    })

    io.icache.req_valid := !io.in.mask                    // 移除对stall的依赖
    io.icache.req_addr  := Cat(io.in.PC(31, 4), 0.U(4.W)) // 16字节对齐

    io.out.req_valid := io.icache.req_valid
    io.out.addr      := io.icache.req_addr
    io.out.inst_valid := io.icache.resp_valid
    io.out.cacheStallRequest := io.icache.stall

    when(io.in.mask) {  // Q: what is `mask` used for? I've forgotten it!
        io.out.Insts := VecInit(Seq.fill(4)(0.U(32.W)))
    }.otherwise {
        io.out.Insts := io.icache.resp_data
    }
}

object InstRAMInterface extends App {
    ChiselStage.emitSystemVerilogFile(
      new InstRAMInterface(),
      Array("--target-dir", "generated")
    )
}
