// Designed by Kyle. 2025-03-13 16:44
import chisel3._
import circt.stage.ChiselStage

/*
    JIRL指令处理器
    当接收到预解码模块的信号，存在JIRL指令时：
    1. 向 RegisterRenaming 模块发起查询，查询重命名后的寄存器
    2. 从 RegisterFile 取出寄存器值
    3. 将寄存器值传递给 Branch_Arbiter

    ** Since V1.0: No longer used **
 */

class JIRL_handler extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val is_jirl = Input(Vec(4, Bool())) // 是否为jirl指令
            val search_result = Input(UInt(7.W))
            val RDJ = Input(UInt(32.W))
            val RJ = Input(UInt(5.W))
        }
        val out = new Bundle {
            val pause = Output(Bool())
            val RAJ = Output(UInt(7.W))
            val map_search = Output(UInt(5.W))
            val RDJ_out = Output(UInt(32.W))
            val RDJ_ready = Output(Bool())
        }
    })

    val regPause = RegInit(false.B)
    val regRDJ_ready = RegInit(false.B)
    io.out.pause := false.B
    io.out.RDJ_ready := false.B
    io.out.RAJ := 0.U
    io.out.RDJ_out := 0.U
    io.out.map_search := 0.U

    when(io.in.is_jirl.asUInt.orR) {
        regPause := true.B
        io.out.map_search := io.in.RJ
        io.out.RAJ := io.in.search_result
        io.out.RDJ_out := io.in.RDJ
    }

    when(regPause) {
        regPause := false.B
        regRDJ_ready := true.B
    }

    io.out.RDJ_ready := regRDJ_ready
    io.out.pause := regPause
}

object JIRL_handler extends App {
    ChiselStage.emitSystemVerilogFile(
      new JIRL_handler(),
      Array("--target-dir", "generated")
    )
}
