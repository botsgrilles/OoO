import chisel3._
import chisel3.util._

class BRAMInterface extends Bundle {
  val addr = Output(UInt(32.W))
  val en = Output(Bool())
  val dout = Input(Vec(4, UInt(32.W)))  // 一次读取4条指令
}