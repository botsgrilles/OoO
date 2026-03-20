// Designed by Kyle. 2025-03-15 08:34
import chisel3._
import circt.stage.ChiselStage
import chisel3.util.PopCount
import chisel3.util.PriorityEncoder
import chisel3.util.log2Ceil
import CONSTANTS.CONFIG.debugSignal

/*
    寄存器重命名
    可同时完成四条指令的重命名
    在上升沿完成重命名，并通过RAW和WAW检查；在下降沿更新FreeList
    srcL----> [mapped(reg)] ----(check)-----> renamed
 */
class RAT(nPsyReg: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val update = Vec(
              4,
              new Bundle {
                  val src     = Input(UInt(5.W))
                  val renamed = Input(UInt(log2Ceil(nPsyReg).W))
              }
            )
            val updateEnable = Vec(4, Input(Bool()))
            val commited     = Vec(4, Input(UInt(log2Ceil(nPsyReg).W)))
            val dest_origin  = Vec(4, Input(UInt(5.W)))
            val dest_old     = Vec(4, Input(UInt(log2Ceil(nPsyReg).W)))
            val commitEnable = Vec(4, Input(Bool()))
            val recover      = Input(Bool())
        }
        val out = new Bundle {
            val freeRegs  = Output(Vec(4, UInt(log2Ceil(nPsyReg).W))) // 可分配的4个物理寄存器
            val freeCount = Output(UInt((log2Ceil(nPsyReg) + 1).W))   // 空闲寄存器数量
            val reverseMapTable =
                Output(Vec(32, UInt(log2Ceil(nPsyReg).W))) // 逻辑寄存器对应的物理寄存器
        }
    })

    val registerAliasTableValidC = RegInit(
      VecInit(Seq.fill(nPsyReg)(false.B))
    ) // 提交的RAT有效位
    val reverseMapTable  = RegInit(VecInit(Seq.fill(32)(0.U(log2Ceil(nPsyReg).W))))
    val reverseMapTableC = RegInit(VecInit(Seq.fill(32)(0.U(log2Ceil(nPsyReg).W))))

    // FIFO队列结构，存放空闲物理寄存器号
    val freeRegQueue = Reg(Vec(nPsyReg - 1, UInt(log2Ceil(nPsyReg).W))) // 存放空闲寄存器号的队列
    val headPtr      = RegInit(0.U(log2Ceil(nPsyReg).W))                // 队列头指针
    // val tailPtr      = RegInit((nPsyReg - 1).U(log2Ceil(nPsyReg).W))    // 队列尾指针
    val tailPtr   = RegInit(0.U(log2Ceil(nPsyReg).W))             // 队列头指针
    val queueSize = RegInit((nPsyReg - 1).U(log2Ceil(nPsyReg).W)) // 当前队列中元素个数

    // 初始化FIFO队列，填入1到127号物理寄存器（0号寄存器始终不可用）
    when(reset.asBool) {
        for (i <- 0 until nPsyReg - 1) {
            freeRegQueue(i) := (i + 1).U(log2Ceil(nPsyReg).W)
        }
    }

    debugSignal(registerAliasTableValidC)

    // 提供4个可用的物理寄存器
    for (i <- 0 until 4) {
        io.out.freeRegs(i) := freeRegQueue((headPtr +& i.U) % (nPsyReg - 1).U)
    }

    io.out.freeCount := queueSize

    when(io.in.recover) {
        reverseMapTable    := reverseMapTableC
        val newQueueSize = (nPsyReg - 1).U - PopCount(registerAliasTableValidC.drop(1))
        debugSignal(newQueueSize)
        queueSize := newQueueSize
        headPtr   := ((tailPtr +& (nPsyReg - 1).U) -& newQueueSize) % (nPsyReg - 1).U
    }.otherwise {
        // 处理更新请求
        for (i <- 0 until 4) {
            when(io.in.updateEnable(i) && io.in.update(i).renamed =/= 0.U) {

                reverseMapTable(io.in.update(i).src) := io.in.update(i).renamed
                val oldPsyReg = reverseMapTable(io.in.update(i).src)
            }
        }

        val realUpdate = Wire(Vec(4, Bool()))
        for (i <- 0 until 4) {
            realUpdate(i) := io.in.updateEnable(i) && io.in.update(i).renamed =/= 0.U
        }

        // 计算这个周期分配的寄存器数量（减少量）
        val numAllocated = PopCount(realUpdate)

        // 先更新headPtr
        when(numAllocated > 0.U) {
            headPtr := (headPtr +& numAllocated) % (nPsyReg - 1).U
        }

        // 跟踪释放的寄存器数量
        val freedVec = Wire(Vec(4, Bool())) // 每条指令是否释放寄存器
        freedVec := VecInit(Seq.fill(4)(false.B))

        // 处理提交请求
        for (i <- 0 until 4) {
            when(io.in.commitEnable(i)) {
                val logicReg   = io.in.dest_origin(i)
                val newPsyReg  = io.in.commited(i)
                val oldPsyReg  = io.in.dest_old(i)
                val shouldFree = oldPsyReg =/= newPsyReg && oldPsyReg =/= 0.U

                // 更新映射表
                reverseMapTableC(logicReg)          := newPsyReg
                registerAliasTableValidC(newPsyReg) := true.B

                // 释放旧的物理寄存器
                when(shouldFree) {
                    registerAliasTableValidC(oldPsyReg) := false.B
                    freedVec(i)                         := true.B
                }
            }
        }

        // 批量处理释放的寄存器，写入队列
        val freedRegs = Wire(Vec(4, UInt(log2Ceil(nPsyReg).W)))
        for (i <- 0 until 4) {
            freedRegs(i) := Mux(freedVec(i), io.in.dest_old(i), 0.U)
        }

        // 使用当前的tailPtr值计算新的写入位置
        val currentTailPtr = tailPtr
        for (i <- 0 until 4) {
            val writeIndex =
                (currentTailPtr +& PopCount(freedVec.take(i))) % (nPsyReg - 1).U
            when(freedVec(i)) {
                freeRegQueue(writeIndex) := freedRegs(i)
            }
        }
        val numFreed = PopCount(freedVec) // 释放的寄存器数量
        tailPtr := (tailPtr +& numFreed) % (nPsyReg - 1).U
        debugSignal(numFreed)
        debugSignal(numAllocated)

        // 一次性更新队列大小，考虑分配和释放
        queueSize := queueSize +& numFreed -& numAllocated
    }

    io.out.reverseMapTable := reverseMapTable
}

class RegisterRenaming(combinational: Boolean, nPsyReg: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val src = Vec(
              4,
              new Bundle {
                  val srcL = Input(UInt(5.W))
                  val srcR = Input(UInt(5.W))
                  val dest = Input(UInt(5.W))
              }
            )
            val recover       = Input(Bool())
            val commited      = Vec(4, Input(UInt(log2Ceil(nPsyReg).W))) // 来自 ROB
            val dest_old      = Vec(4, Input(UInt(log2Ceil(nPsyReg).W)))
            val dest_origin   = Vec(4, Input(UInt(5.W)))
            val commitEnable  = Vec(4, Input(Bool()))                    // 来自 ROB
            val pauseRenaming = Input(Bool())
            val valid         = Input(Vec(4, Bool()))                    // 指令是否有效
        }
        val out = new Bundle {
            val renamed = Vec(
              4,
              new Bundle {
                  val dest = Output(UInt(log2Ceil(nPsyReg).W))
                  val srcL = Output(UInt(log2Ceil(nPsyReg).W))
                  val srcR = Output(UInt(log2Ceil(nPsyReg).W))
              }
            )
            val dest_old     = Vec(4, UInt(log2Ceil(nPsyReg).W))
            val freeListFull = Output(Bool())
            val valid = Output(Vec(4, Bool()))
        }
    })

    val rat = Module(new RAT(nPsyReg = nPsyReg))
    rat.io.in.recover      := io.in.recover
    rat.io.in.commitEnable := io.in.commitEnable
    rat.io.in.dest_origin  := io.in.dest_origin
    rat.io.in.dest_old     := io.in.dest_old
    rat.io.in.commited     := io.in.commited

    val Mapped = if (combinational) {
        VecInit(Seq.fill(4)(0.U.asTypeOf(new Bundle {
            val srcL = UInt(log2Ceil(nPsyReg).W)
            val srcR = UInt(log2Ceil(nPsyReg).W)
            val dest = UInt(log2Ceil(nPsyReg).W)
        })))
    } else {
        RegInit(VecInit(Seq.fill(4)(0.U.asTypeOf(new Bundle {
            val srcL = UInt(log2Ceil(nPsyReg).W)
            val srcR = UInt(log2Ceil(nPsyReg).W)
            val dest = UInt(log2Ceil(nPsyReg).W)
        }))))
    }

    // 检查是否有足够的空闲寄存器
    val nonZeroDestCount = PopCount(
      io.in.src.zip(io.in.valid).map { case (src, valid) => src.dest =/= 0.U && valid }
    )
    val freeListFull = nonZeroDestCount > rat.io.out.freeCount

    io.out.freeListFull := freeListFull
    /* --------- Step 1a : 映射源寄存器 --------- */
    for (i <- 0 until 4) {
        when(io.in.src(i).srcL === 0.U || io.in.valid(i) === false.B || freeListFull) {
            Mapped(i).srcL := 0.U
        }.otherwise {
            Mapped(i).srcL := rat.io.out.reverseMapTable(io.in.src(i).srcL)
        }
        when(io.in.src(i).srcR === 0.U || io.in.valid(i) === false.B || freeListFull) {
            Mapped(i).srcR := 0.U
        }.otherwise {
            Mapped(i).srcR := rat.io.out.reverseMapTable(io.in.src(i).srcR)
        }
    }

    /* --------- Step 2a : 检查 (需要维持) RAW 相关性 --------- */
    io.out.renamed(0).srcL := Mapped(0).srcL
    io.out.renamed(0).srcR := Mapped(0).srcR
    io.out.renamed(0).dest := Mapped(0).dest
    for (i <- 1 until 4) {
        io.out.renamed(i).srcL := Mapped(i).srcL
        io.out.renamed(i).srcR := Mapped(i).srcR
        io.out.renamed(i).dest := Mapped(i).dest
        for (j <- 0 until i) {
            when(io.in.src(i).srcL === io.in.src(j).dest && io.in.valid(j)) {
                io.out.renamed(i).srcL := io.out.renamed(j).dest
            }
            when(io.in.src(i).srcR === io.in.src(j).dest && io.in.valid(j)) {
                io.out.renamed(i).srcR := io.out.renamed(j).dest
            }
        }
    }

    /* --------- Step 1b : 映射目的寄存器 --------- */
    /* ---------- Step 2b : 检查 WAW 相关性 ---------- */
    when(!io.in.pauseRenaming) {
        // 判断每条指令是否需要分配目标寄存器
        val needsDestReg = Wire(Vec(4, Bool()))
        val WAWSatisfied = Wire(Vec(4, Bool()))

        for (i <- 0 until 4) {
            needsDestReg(i) := io.in.valid(i) && io.in.src(i).dest =/= 0.U
        }

        // 正确的WAW检测：检查当前指令是否与后续指令有WAW冲突
        for (i <- 0 until 4) {
            val hasWAWConflict = (i + 1 until 4)
                .map { j =>
                    io.in.valid(j) && io.in.src(i).dest === io.in.src(j).dest && io.in
                        .src(i)
                        .dest =/= 0.U
                }
                .foldLeft(false.B)(_ || _)

            WAWSatisfied(i) := !hasWAWConflict
        }

        WAWSatisfied := VecInit(Seq.fill(4)(true.B))

        val finalAssignments = needsDestReg.zip(WAWSatisfied).map {
            case (needs, noConflict) => needs && noConflict
        }

        val needsPrefixSum = Wire(Vec(4, UInt(2.W)))

        debugSignal(WAWSatisfied)
        debugSignal(needsDestReg)

        needsPrefixSum(0) := 0.U
        for (i <- 1 until 4) {
            needsPrefixSum(i) := needsPrefixSum(i - 1) + finalAssignments(i - 1).asUInt
        }

        // 创建压缩后的映射表，用于RAT更新
        val compressedSrc  = Wire(Vec(4, UInt(5.W)))
        val compressedDest = Wire(Vec(4, UInt(log2Ceil(nPsyReg).W)))

        // 初始化为0
        for (i <- 0 until 4) {
            compressedSrc(i)  := 0.U
            compressedDest(i) := 0.U
        }

        val computedDestOld = Wire(Vec(4, UInt(log2Ceil(nPsyReg).W)))

        for (i <- 0 until 4) {
            // 默认使用RAT中的映射
            computedDestOld(i) := rat.io.out.reverseMapTable(io.in.src(i).dest)

            // 检查是否有前面的指令已经重命名了同一个逻辑寄存器
            for (j <- 0 until i) {
                when(
                  io.in.valid(j) && io.in.src(j).dest === io.in.src(i).dest && io.in
                      .src(j)
                      .dest =/= 0.U
                ) {
                    computedDestOld(i) := io.out.renamed(j).dest
                }
            }
        }

        for (i <- 0 until 4) {
            val dest =
                Mux(finalAssignments(i), rat.io.out.freeRegs(needsPrefixSum(i)), 0.U)

            // 对于有WAW冲突的指令，使用前面指令分配的物理寄存器
            when(!WAWSatisfied(i) && needsDestReg(i)) {
                // 找到最后一个写同一逻辑寄存器的指令
                var lastWriter = 0.U
                for (j <- 0 until i) {
                    when(
                      io.in.valid(j) && io.in.src(j).dest === io.in.src(i).dest && io.in
                          .src(i)
                          .dest =/= 0.U
                    ) {
                        lastWriter = io.out.renamed(j).dest
                    }
                }
                io.out.renamed(i).dest := lastWriter
            }.otherwise {
                io.out.renamed(i).dest := dest
            }

            // 只有真正分配新寄存器的指令才输出有效的dest_old
            io.out.dest_old(i) := Mux(finalAssignments(i), computedDestOld(i), 0.U)

            // 如果当前指令需要分配寄存器，则添加到压缩数组中
            when(finalAssignments(i)) {
                compressedSrc(needsPrefixSum(i))  := io.in.src(i).dest
                compressedDest(needsPrefixSum(i)) := dest
            }
        }

        // 使用压缩后的数据更新RAT
        for (i <- 0 until 4) {
            rat.io.in.update(i).src     := compressedSrc(i)
            rat.io.in.update(i).renamed := compressedDest(i)
        }

        val totalAllocations = needsPrefixSum(3) +& finalAssignments(3).asUInt

        debugSignal(totalAllocations)

        rat.io.in.updateEnable := VecInit(Seq.tabulate(4)(i => i.U < totalAllocations))
    }.otherwise {
        for (i <- 0 until 4) {
            rat.io.in.updateEnable(i)   := false.B
            rat.io.in.update(i).src     := 0.U
            rat.io.in.update(i).renamed := 0.U
            io.out.renamed(i).dest      := 0.U
            io.out.dest_old(i)          := 0.U
        }
    }

    io.out.valid := Mux(freeListFull, VecInit(Seq.fill(4)(false.B)), io.in.valid)
    debugSignal(io.out.valid)
}

object RegisterRenaming extends App {
    ChiselStage.emitSystemVerilogFile(
      new RegisterRenaming(combinational = true, nPsyReg = 64),
      Array("--target-dir", "generated")
    )
}
