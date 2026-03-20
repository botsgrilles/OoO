// Designed by Kyle. 2025-02-25 18:39
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import CONSTANTS.BRANCH_TYPE
import CONSTANTS.CONFIG.enableBranchPrediction
import CONSTANTS.CONFIG.debugSignal

// TODO: Performance tests & LRU Cache in RAS

object hashUtils {
    def hashA(pc: UInt, nEntries: Int): UInt = {
        val indexBits = log2Up(nEntries)
        // pc[6:3] ^ pc[11:8] ^ pc[19:16] ^ pc[23:20]，再取低indexBits位
        val part1 = pc(9, 4)
        val part2 = pc(10, 5)
        val part3 = pc(14, 9)
        val part4 = pc(17, 12)
        val mixed = part1 ^ part2 ^ part3 ^ part4
        mixed(indexBits - 1, 0)
    }

    def hashB(pc: UInt, nEntries: Int): UInt = {
        val indexBits = log2Up(nEntries)
        val part1     = Reverse(pc(9, 4))
        val part2     = Reverse(pc(10, 5))
        val part3     = Reverse(pc(14, 9))
        val part4     = Reverse(pc(17, 12))
        val mixed     = part1 ^ part2 ^ part3 ^ part4
        mixed(indexBits - 1, 0)
    }
}

/*
    分支方向预测器 (P111)
    使用局部历史的预测方法，根据hash后的PC从BHT中找到局部历史，与PC一部分拼接后索引PHT
    PHT中存储了分支预测器的状态机，使用双饱和计数器实现
 */
class DirectionPredictor(nBHTEntries: Int, nPHTEntries: Int) extends Module {
    val io = IO(new Bundle {
        val PC_in                   = Input(UInt(32.W)) // 程序计数器
        val PC_where_branched       = Input(UInt(32.W)) // 更新的PC
        val direction_update        = Input(Bool())     // 实际分支方向
        val direction_update_enable = Input(Bool())     // 分支更新使能
        val direction_predicted     = Output(Bool())    // 预测的目标地址
    })
    val direction_predicted = WireInit(false.B) // 预测的目标地址
    val BHT = RegInit(VecInit(Seq.fill(nBHTEntries)(0.U(4.W)))) // Branch History Table
    val PHT = RegInit(
      VecInit(Seq.fill(nPHTEntries)(1.U(2.W)))
    ) // Pattern History Table，存储双饱和计数器状态

    val PC_aligned = Cat(io.PC_in(31, 4), 0.U(4.W))           // PC对齐
    val BHT_index  = hashUtils.hashA(PC_aligned, nBHTEntries) // BHT索引

    debugSignal(BHT_index)

    val PHT_index = Cat(BHT(BHT_index), PC_aligned(6, 4)) // PHT索引

    // 预测逻辑：状态为2或3时预测跳转
    direction_predicted := PHT(PHT_index) >= 2.U

    debugSignal(PC_aligned)
    debugSignal(PHT_index)

    when(io.direction_update_enable) {
        val PC_update_aligned =
            Cat(io.PC_where_branched(31, 4), 0.U(4.W)) // 更新的PC对齐

        debugSignal(PC_update_aligned)

        val update_BHT_index = hashUtils.hashA(PC_update_aligned, nBHTEntries)
        val update_PHT_index = Cat(BHT(update_BHT_index), PC_update_aligned(6, 4))

        // 更新BHT
        BHT(update_BHT_index) := Cat(BHT(update_BHT_index)(2, 0), io.direction_update)

        // 更新PHT中的双饱和计数器
        when(io.direction_update) {
            // 跳转：状态加1，最大为3
            PHT(update_PHT_index) := Mux(
              PHT(update_PHT_index) === 3.U,
              3.U,
              PHT(update_PHT_index) + 1.U
            )
        }.otherwise {
            // 不跳转：状态减1，最小为0
            PHT(update_PHT_index) := Mux(
              PHT(update_PHT_index) === 0.U,
              0.U,
              PHT(update_PHT_index) - 1.U
            )
        }
    }

    io.direction_predicted := direction_predicted
}

/*
    Return Address Stack (RAS) 返回地址栈
    满堆栈
 */
class RAS(depth: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val addr = Input(UInt(32.W)) // 目标地址
            val push = Input(Bool())     // 入栈
            val pop  = Input(Bool())     // 出栈
        }
        val out = new Bundle {
            val addr_out = Output(UInt(32.W)) // 栈顶的目标地址
            val empty    = Output(Bool())     // 栈是否为空
        }

    })
    val stack = RegInit(VecInit(Seq.fill(depth)(0.U(32.W)))) // 返回地址栈
    val sp    = RegInit(0.U(log2Ceil(depth).W))              // 栈指针
    val count = RegInit(0.U(log2Ceil(depth + 1).W))          // 栈中元素个数

    when(io.in.push) {
        stack(sp) := io.in.addr
        sp        := Mux(sp === (depth - 1).U, 0.U, sp + 1.U)
        count     := Mux(count === depth.U, depth.U, count + 1.U)
    }.elsewhen(io.in.pop && count =/= 0.U) {
        sp    := Mux(sp === 0.U, (depth - 1).U, sp - 1.U)
        count := count - 1.U
    }

    io.out.addr_out := Mux(
      count === 0.U,
      0.U,
      stack(Mux(sp === 0.U, (depth - 1).U, sp - 1.U))
    )
    io.out.empty := count === 0.U
}

/*
    分支目标地址预测器
    对于直接跳转类型 (bne, beq, b), 使用 BTB 缓存记录跳转的地址
    对于间接跳转类型 (bl, jirl), 使用 RAS 缓存记录跳转的地址
 */
class TargetPredictor(tagWidth: Int, nCacheEntries: Int) extends Module {
    val io = IO(new Bundle {
        val PC_in                = Input(UInt(32.W)) // 程序计数器
        val PC_where_branched    = Input(UInt(32.W)) // 更新的PC
        val target_update        = Input(UInt(32.W)) // 更新的目标地址
        val target_update_enable = Input(Bool())     // 目标地址更新使能
        val branch_type      = Input(UInt(2.W))   // 分支类型：00：直接跳转，01：间接跳转，10：函数调用，11：函数返回
        val target_predicted = Output(UInt(32.W)) // 预测的目标地址
        val offset_predicted = Output(UInt(2.W))  // 预测的偏移量 (四条指令中，跳转指令是哪一条？)
        val condition_predicted = Output(Bool()) // 是否是条件分支
        val pause_PC_update     = Input(Bool())
        val valid               = Output(Bool()) // 是否有效
    })

    // |  valid  |     tag     |  target   |  offset  | branch_type |
    // |  有效位  |标记 (当前PC) |  目标地址  |  偏移量   |   分支类型   |
    val BTB = RegInit(VecInit(Seq.fill(nCacheEntries)(0.U.asTypeOf(new Bundle {
        val valid       = Bool()
        val tag         = UInt(6.W)
        val target      = UInt(32.W)
        val offset      = UInt(2.W) // 四条指令中，跳转指令是哪一条？
        val branch_type = UInt(2.W)
    }))))

    val RAS = Module(new RAS(16)).io // 返回地址栈
    RAS.in.push := false.B
    RAS.in.pop  := false.B
    RAS.in.addr := 0.U

    val target_predicted    = WireInit(0.U(32.W)) // 预测的目标地址
    val offset_predicted    = WireInit(3.U(2.W))
    val condition_predicted = WireInit(false.B)   // 是否是条件分支
    val valid               = WireInit(false.B)   // 是否有效

    val PC_aligned        = Cat(io.PC_in(31, 4), 0.U(4.W))                    // PC对齐
    val PC_update_aligned = Cat(io.PC_where_branched(31, 4), 0.U(4.W))        // 更新的PC对齐
    val PC_update_offset  = io.PC_where_branched(3, 2)
    val index             = hashUtils.hashB(PC_aligned, nCacheEntries)        // 取tag
    val update_index      = hashUtils.hashB(PC_update_aligned, nCacheEntries) // 取tag
    val tag               = PC_aligned(4 + tagWidth - 1, 4)                   // 取index
    val update_tag        = PC_update_aligned(4 + tagWidth - 1, 4)            // 取index

    debugSignal(PC_aligned)
    debugSignal(PC_update_aligned)
    debugSignal(index)
    debugSignal(update_index)
    debugSignal(tag)
    debugSignal(offset_predicted)

    // 接受实际目标地址的更新
    when(io.target_update_enable) {
        BTB(update_index).valid       := true.B
        BTB(update_index).tag         := update_tag
        BTB(update_index).target      := io.target_update
        BTB(update_index).offset      := PC_update_offset // 四条指令中，跳转指令是哪一条？
        BTB(update_index).branch_type := io.branch_type
        // RAS在更新阶段不操作，只在预测阶段操作
    }

    // 把未跳转情况下的后一个PC值作为 fallback
    when(BTB(index).valid && BTB(index).tag === tag) {
        target_predicted := Mux(
          BTB(index).branch_type =/= BRANCH_TYPE.RETURN,
          BTB(index).target,
          Mux(!RAS.out.empty, RAS.out.addr_out, Cat(io.PC_in(31, 4) + 1.U, 0.U(4.W)))
        )
        offset_predicted    := BTB(index).offset
        condition_predicted := BTB(index).branch_type === BRANCH_TYPE.CONDITION
        valid               := true.B // 有效
        // 在预测阶段进行RAS操作
        when(
          BTB(index).branch_type === BRANCH_TYPE.CALL &&
              io.PC_in(3, 2).asUInt <= offset_predicted &&
              !io.pause_PC_update
        ) {
            RAS.in.push := true.B
            // RAS.in.addr := io.PC_in + 4.U // push返回地址（当前PC+4）
            RAS.in.addr := Cat(
              io.PC_in(31, 4) + BTB(index).offset,
              0.U(4.W)
            ) + 4.U                                                 // push返回地址（当前PC+4）
        }.elsewhen(BTB(index).branch_type === BRANCH_TYPE.RETURN) { // 预测到函数返回
            RAS.in.pop := true.B
        }.otherwise {
            RAS.in.push := false.B
            RAS.in.pop  := false.B
            RAS.in.addr := 0.U
        }
    }.otherwise {
        target_predicted := Cat(io.PC_in(31, 4) + 1.U, 0.U(4.W))
        offset_predicted := 3.U
        valid            := false.B
        RAS.in.push      := false.B
        RAS.in.pop       := false.B
        RAS.in.addr      := 0.U
    }

    io.target_predicted    := target_predicted
    io.offset_predicted    := offset_predicted
    io.condition_predicted := condition_predicted
    io.valid               := valid
}

class BranchPrediction extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val PC                = Input(UInt(32.W)) // 程序计数器
            val PC_where_branched = Input(UInt(32.W))
            val target_true       = Input(UInt(32.W)) // 实际目标地址
            val update_enable     = Input(Bool())     // 更新使能
            val direction_true    = Input(Bool())     // 实际分支方向
            val branch_type       = Input(UInt(2.W))  // 分支类型
            val pause_PC_update   = Input(Bool())     // 暂停PC更新
        }
        val out = new Bundle {
            val direction_predicted = Output(Bool())     // 分支预测结果
            val target_predicted    = Output(UInt(32.W)) // 目标地址预测结果
            val offset_predicted    = Output(UInt(2.W))
            val condition_predicted = Output(Bool())     // 是否是条件分支
            val valid               = Output(Bool())     // 是否有效
        }
    })

    if (enableBranchPrediction) {
        val directionPredictor = Module(new DirectionPredictor(8, 128)).io
        val targetPredictor    = Module(new TargetPredictor(8, 64)).io
        directionPredictor.PC_in                   := io.in.PC
        directionPredictor.PC_where_branched       := io.in.PC_where_branched
        directionPredictor.direction_update        := io.in.direction_true
        directionPredictor.direction_update_enable := io.in.update_enable

        targetPredictor.PC_in                := io.in.PC
        targetPredictor.PC_where_branched    := io.in.PC_where_branched
        targetPredictor.target_update        := io.in.target_true
        targetPredictor.target_update_enable := io.in.update_enable && io.in.direction_true
        targetPredictor.branch_type          := io.in.branch_type
        targetPredictor.pause_PC_update      := io.in.pause_PC_update

        io.out.direction_predicted := directionPredictor.direction_predicted && targetPredictor.valid
        io.out.target_predicted := Mux(
          directionPredictor.direction_predicted,
          targetPredictor.target_predicted,
          Cat(io.in.PC(31, 4) + 1.U, 0.U(4.W))
        )
        io.out.offset_predicted := Mux(
          targetPredictor.valid && io.out.direction_predicted,
          targetPredictor.offset_predicted,
          3.U // 如果没有预测到跳转，则偏移量为3，表示下一条指令
        )
        io.out.condition_predicted := targetPredictor.condition_predicted
        io.out.valid               := targetPredictor.valid
    } else {
        io.out.direction_predicted := false.B
        io.out.target_predicted    := Cat(io.in.PC(31, 4) + 1.U, 0.U(4.W))
        io.out.offset_predicted    := 3.U
        io.out.condition_predicted := false.B
        io.out.valid               := false.B // 分支预测器未启用时，默认无效
    }
}

object BranchPrediction extends App {
    ChiselStage.emitSystemVerilogFile(
      new BranchPrediction(),
      Array("--target-dir", "generated")
    )
}
