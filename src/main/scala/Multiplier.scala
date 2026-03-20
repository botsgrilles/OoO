// Designed by Kyle. 2025-04-21 09:05
import scala.Console
import chisel3._
import chisel3.util.HasBlackBoxInline // 导入 BlackBox 相关工具
import circt.stage.ChiselStage
import chisel3.util.ShiftRegister
import CONSTANTS.EXECUTION_CYCLE

// 定义 Vivado IP 核的 BlackBox 接口
// **重要**: 将 "your_vivado_multiplier_ip_name" 替换为你的 Vivado IP 核的实际 Verilog 模块名
// **重要**: 确认 Vivado IP 核的端口名称 (A, B, P, CLK) 和位宽是否与此处定义一致
class VivadoMultiplierIP extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
        val CLK = Input(Clock()) // 时钟输入
        val A = Input(UInt(32.W)) // 输入操作数 A
        val B = Input(UInt(32.W)) // 输入操作数 B
        val P = Output(UInt(64.W)) // 乘法结果 (通常是 64 位)
    })
    // 1. 为此 BlackBox Wrapper 设置一个唯一的 Verilog 模块名
    override def desiredName = "MultiplierVivadoIP_wrapper"

    // 2. setInline 定义 "MultiplierVivadoIP_wrapper" 模块。
    //    此 wrapper 模块实例化你实际的 Vivado IP 核，其模块名为 "multiplier"。
    setInline("VivadoMultiplierIP.v", // 此文件将包含 MultiplierVivadoIP_wrapper 的定义
        s"""module MultiplierVivadoIP_wrapper ( // 模块名必须与 desiredName 匹配
           |    input CLK,
           |    input [31:0] A,
           |    input [31:0] B,
           |    output [63:0] P
           |);
           |
           |  // 实例化实际的 Vivado IP 核。
           |  // 你的 Vivado IP 核的 Verilog 模块名为 "multiplier"。
           |  // 确保定义 "multiplier" 的 Verilog 文件 (来自 Vivado) 已包含在你的综合项目中。
           |  multiplier vivado_ip_instance (
           |    .CLK(CLK),
           |    .A(A),
           |    .B(B),
           |    .P(P)
           |  );
           |
           |endmodule
           |""".stripMargin)
}


class Multiplier(mode: String) extends Module {
    val io = IO(new Bundle {
        // val clock = Input(Clock()) // 移除显式的时钟输入
        val a = Input(UInt(32.W))
        val b = Input(UInt(32.W))
        val result = Output(UInt(32.W)) // 保持 32 位输出，截断结果
    })

    if (mode == "Simulation") {
        // 仿真模式：直接使用 Chisel 乘法操作符
        val fullResult = ShiftRegister(io.a * io.b, EXECUTION_CYCLE.MUL) // 使用 ShiftRegister 模拟时钟延迟
        io.result := fullResult(31, 0)
        println("Multiplier instantiated in \u001B[33mSimulation\u001B[0m mode.")
    } else if (mode == "Synthesis") {
        // 综合模式：实例化 Vivado IP 核的 BlackBox
        val vivadoMult = Module(new VivadoMultiplierIP())
        // 使用模块的隐式时钟 clock
        vivadoMult.io.CLK := clock
        vivadoMult.io.A := io.a
        vivadoMult.io.B := io.b
        io.result := vivadoMult.io.P(31, 0)
        println("Multiplier instantiated in \u001B[33mSynthesis\u001B[0m mode.")
    } else {
        sys.error(s"Invalid mode '$mode' for Multiplier. Use 'Simulation' or 'Synthesis'.")
        io.result := 0.U
    }
}


object Multiplier extends App {
    ChiselStage.emitSystemVerilogFile(
        gen = new Multiplier(mode = "Simulation"), // Directly instantiate the module
        // gen = new Multiplier(mode = "Synthesis"), // Directly instantiate the module
        args = Array("--target-dir", "generated")
    )
}