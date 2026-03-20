import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline

class BlockMemory(
  readonly: Boolean,
  width: Int,
  depth: Int,
  initFile: Option[String] = None
) extends Module {
    // Ensure these imports are at the top of your Scala file:

    val io = IO(new Bundle {
        val en = Input(Bool()) // Enable for read/write operation
        // Ensure depth is at least 1, as log2Ceil(0) is undefined.
        val addr = Input(UInt(chisel3.util.log2Ceil(depth).W)) // Address input
        val dout = Output(UInt(width.W))                       // Data output

        // Conditional ports for writing, only if not readonly
        val we  = if (!readonly) Some(Input(Bool())) else None        // Write enable
        val din = if (!readonly) Some(Input(UInt(width.W))) else None // Data input
    })

    // Instantiate a synchronous read memory.
    // Reads are synchronous: data output is delayed by one clock cycle from address/enable assertion.
    // Writes are synchronous: data is written on the clock edge when write enable is asserted.
    val mem = SyncReadMem(depth, UInt(width.W))

    // Initialize memory from file if initFile path is provided in the constructor.
    // This requires `initFile: Option[String]` to be a parameter of the BlockMemory class.
    initFile.foreach { path =>
        loadMemoryFromFileInline(mem, path)
    }

    // Read operation
    // Data read from mem(io.addr) will appear on io.dout in the clock cycle *after* io.en is asserted.
    io.dout := mem.read(io.addr, io.en)

    // Write operation (only if memory is not read-only)
    if (!readonly) {
        // io.we.get and io.din.get are safe because these ports only exist if !readonly.
        when(io.en && io.we.get) {
            mem.write(io.addr, io.din.get)
        }
    }
}

object BlockMemory extends App {
    ChiselStage.emitSystemVerilogFile(
      new BlockMemory(readonly = false, width = 32, depth = 1024, initFile = Some("../simulation/default0.hex")),
      Array("--target-dir", "generated")
    )
}
