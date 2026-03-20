// Designed by Kyle. 2025-03-13 18:10
import chisel3._
import circt.stage.ChiselStage
import chisel3.util.log2Ceil

/*
    通用寄存器
    8个读端口，无需使能信号（支持并行读取，遵循时钟）
        Src0L ----> RD0L    Src0R ----> RD0R
        Src0L ----> RD1L    Src0R ----> RD1R
        Src0L ----> RD2L    Src0R ----> RD2R
        Src0L ----> RD3L    Src0R ----> RD3R
    4个写端口，有使能信号（支持并行读取，遵循时钟）
        WD0 ----WE0----> (Dest0)
        WD1 ----WE1----> (Dest1)
        WD2 ----WE2----> (Dest2)
        WD3 ----WE3----> (Dest3)
    0号寄存器始终为0，写入无效。
 */
class RegisterFile(nPsyRegs: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val CommonPorts = Vec(
              4,
              new Bundle {
                  val srcL = Input(UInt(log2Ceil(nPsyRegs).W))
                  val srcR = Input(UInt(log2Ceil(nPsyRegs).W))
                  val dest = Input(UInt(log2Ceil(nPsyRegs).W))
                  val WE   = Input(Bool())     // Write Enable
                  val WD   = Input(UInt(32.W)) // Write Data
              }
            )
            val stall = Vec(4, Input(Bool()))
        }
        val out = new Bundle {
            val CommonReadL = Vec(4, UInt(32.W))
            val CommonReadR = Vec(4, UInt(32.W))
        }
    })

    val regFile = RegInit(VecInit(Seq.fill(nPsyRegs)(0.U(32.W))))

    for (i <- 0 until 4) {
        val srcL_addr = io.in.CommonPorts(i).srcL
        val srcR_addr = io.in.CommonPorts(i).srcR

        // Initialize with value from register file
        val valL = Wire(UInt(32.W))
        val valR = Wire(UInt(32.W))

        val regvalL = RegInit(0.U(32.W))
        val regvalR = RegInit(0.U(32.W))

        // Default to reading from regFile (combinational read)
        valL := Mux(srcL_addr === 0.U, 0.U, regFile(srcL_addr))
        valR := Mux(srcR_addr === 0.U, 0.U, regFile(srcR_addr))

        // Forwarding logic: iterate write ports.
        // Later assignments in this loop (higher 'j') will override earlier ones
        // if they target the same read address, matching the write logic's priority.
        for (j <- 0 until 4) {
            when(io.in.CommonPorts(j).WE && io.in.CommonPorts(j).dest =/= 0.U) {
                when(io.in.CommonPorts(j).dest === srcL_addr && srcL_addr =/= 0.U) {
                    valL := io.in.CommonPorts(j).WD
                }
                when(io.in.CommonPorts(j).dest === srcR_addr && srcR_addr =/= 0.U) {
                    valR := io.in.CommonPorts(j).WD
                }
            }
        }

        regvalL := Mux(io.in.stall(i),regvalL, valL)
        regvalR := Mux(io.in.stall(i),regvalR, valR)
        io.out.CommonReadL(i) := regvalL
        io.out.CommonReadR(i) := regvalR
    }

    // 写入逻辑
    for (i <- 0 until 4) {
        when(io.in.CommonPorts(i).WE && io.in.CommonPorts(i).dest =/= 0.U) {
            regFile(io.in.CommonPorts(i).dest) := io.in.CommonPorts(i).WD
        }
    }
}

object RegisterFile extends App {
    ChiselStage.emitSystemVerilogFile(
      new RegisterFile(128),
      Array("--target-dir", "generated")
    )
}
