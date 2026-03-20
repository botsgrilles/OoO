import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.ChiselStage

object GenerateVerilog extends App {
  private val firrtlOpts =
    if (args.nonEmpty) args
    else
      Array("--target", "systemverilog", "--split-verilog", "-td", "build/rtl")

  (new ChiselStage).execute(
    firrtlOpts,
    Seq(ChiselGeneratorAnnotation(() => new TOPDesign()))
  )
}