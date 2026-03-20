// Designed by Kyle. 2025-02-27 13:50
import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RATold(nWays: Int, nPsyRegs: Int) extends Module {
    val io = IO(new Bundle {
        val committed = Vec( // 已提交的物理寄存器编号（来自Commit阶段）
          nWays,
          new Bundle {
              val srcL = Input(UInt(log2Ceil(nPsyRegs).W))
              val srcR = Input(UInt(log2Ceil(nPsyRegs).W))
              val dest = Input(UInt(log2Ceil(nPsyRegs).W))
          }
        )

        val update = Vec( // 重命名后更新RAT表
          nWays,
          new Bundle {
              val mapL = new Bundle() {
                  val src = Input(UInt(5.W))
                  val psy = Input(UInt(log2Up(nPsyRegs).W))
              }
              val mapR = new Bundle() {
                  val src = Input(UInt(5.W))
                  val psy = Input(UInt(log2Up(nPsyRegs).W))
              }
              val mapD = new Bundle() {
                  val src = Input(UInt(5.W))
                  val psy = Input(UInt(log2Up(nPsyRegs).W))
              }
          }
        )

        val recover = Input(Bool()) // 恢复指令
        val updateEnable = Input(Bool()) // 更新RAT表信号
        val updatedestEnable = Input(Vec(nWays, Bool())) // 更新目的寄存器使能信号（解决WAW）
        val commitEnable = Input(Bool()) // 提交使能信号

        val prefixSum = Output(Vec(nPsyRegs, UInt(log2Up(nPsyRegs).W))) // 前缀和
        val freeListout = Output(Vec(nPsyRegs, Bool())) // 空闲寄存器列表
        val pause = Output(Bool()) // 暂停流水线
    })

    val regPause = RegInit(false.B) // 暂停流水线信号
    val registerAliasTable = RegInit(VecInit(Seq.fill(nPsyRegs)(0.U(5.W)))) // 物理寄存器别名表
    val freeList = RegInit(VecInit(Seq.fill(nPsyRegs)(true.B))) // 空闲寄存器列表
    val registerAliasTableC = RegInit(
      VecInit(Seq.fill(nPsyRegs)(0.U(5.W)))
    ) // 提交阶段物理寄存器别名表（永远正确）
    val freeListC = RegInit(VecInit(Seq.fill(nPsyRegs)(true.B))) // 提交阶段空闲寄存器列表（永远正确）

    io.freeListout := freeList

    when(io.updateEnable) {
        for (i <- 0 until nWays) {
            registerAliasTable(io.update(i).mapL.psy) := io.update(i).mapL.src
            registerAliasTable(io.update(i).mapR.psy) := io.update(i).mapR.src
            freeList(io.update(i).mapL.psy) := false.B
            freeList(io.update(i).mapR.psy) := false.B
            when(io.updatedestEnable(i)) {
                registerAliasTable(io.update(i).mapD.psy) := io.update(i).mapD.src
                freeList(io.update(i).mapD.psy) := false.B
            }
        }
    }

    // 当还原信号产生时，将cRAT拷贝到RAT
    when(io.recover) {
        registerAliasTable := registerAliasTableC
        freeList := freeListC
    }

    // 当更新信号产生时：根据updated中描述的寄存器编号同步至cRAT
    when(io.commitEnable) {
        for (i <- 0 until nWays) {
            registerAliasTableC(io.committed(i).srcL) := registerAliasTable(
              io.committed(i).srcL
            )
            registerAliasTableC(io.committed(i).srcR) := registerAliasTable(
              io.committed(i).srcR
            )
            registerAliasTableC(io.committed(i).dest) := registerAliasTable(
              io.committed(i).dest
            )
            freeListC(io.committed(i).srcL) := freeList(io.committed(i).srcL)
            freeListC(io.committed(i).srcR) := freeList(io.committed(i).srcR)
            freeListC(io.committed(i).dest) := freeList(io.committed(i).dest)
            // TODO 添加释放旧物理寄存器的逻辑
        }
    }

    // 计算前缀和
    io.prefixSum(0) := false.B // 0号寄存器不可分配，只用于映射逻辑$0
    io.prefixSum(1) := freeList(1)
    for (i <- 2 until nPsyRegs) {
        io.prefixSum(i) := io.prefixSum(i - 1) + freeList(i).asUInt
    }
    // 统计空闲寄存器数量
    when(PopCount(freeList.asUInt) < (3 * nWays).U) {
        regPause := true.B
    }.otherwise {
        regPause := false.B
    }
    io.pause := regPause
}

class RenamingUnit(nWays: Int, nPsyRegs: Int) extends Module {
    val io = IO(new Bundle {
        val src = Vec(
          nWays,
          new Bundle { // 重命名前的寄存器
              val srcL = Input(UInt(5.W)) // 源寄存器1
              val srcR = Input(UInt(5.W)) // 源寄存器2
              val dest = Input(UInt(5.W)) //  目的寄存器
          }
        )
        val update = Vec( // 重命名后更新RAT表
          nWays,
          new Bundle {
              val mapL = new Bundle() {
                  val src = Output(UInt(5.W))
                  val psy = Output(UInt(log2Up(nPsyRegs).W))
              }
              val mapR = new Bundle() {
                  val src = Output(UInt(5.W))
                  val psy = Output(UInt(log2Up(nPsyRegs).W))
              }
              val mapD = new Bundle() {
                  val src = Output(UInt(5.W))
                  val psy = Output(UInt(log2Up(nPsyRegs).W))
              }
          }
        )
        val freeList = Input(Vec(nPsyRegs, Bool())) // 空闲寄存器列表
        val prefixSum = Input(Vec(nPsyRegs, UInt(log2Up(nPsyRegs).W))) // 前缀和
        val pause = Input(Bool()) // 暂停流水线
        val renamed = Vec(
          nWays,
          new Bundle { // 重命名后的寄存器
              val srcL = Output(UInt(log2Up(nPsyRegs).W))
              val srcR = Output(UInt(log2Up(nPsyRegs).W))
              val dest = Output(UInt(log2Up(nPsyRegs).W))
          }
        )
        val updateEnable = Output(Bool()) // 更新RAT表信号
        val updatedestEnable = Output(Vec(nWays, Bool())) // 更新目的寄存器使能信号（解决WAW）
    })
    val RegUpdateEnable = RegInit(false.B) // 更新RAT表信号
    val mapped = Wire( // 第一阶段重命名
      Vec(
        nWays,
        new Bundle {
            val srcL = UInt(log2Up(nPsyRegs).W)
            val srcR = UInt(log2Up(nPsyRegs).W)
            val dest = UInt(log2Up(nPsyRegs).W)
        }
      )
    )

    // 重命名第一阶段：根据prefixSum计算重命名后的寄存器编号 (src -> mapped)
    when(!io.pause) {
        val allocIndices = Wire(Vec(nWays * 3, UInt(log2Up(nPsyRegs).W)))
        for (k <- 0 until nWays * 3) {
            allocIndices(k) := PriorityEncoder(
              io.freeList.asUInt & VecInit(io.prefixSum.zipWithIndex.map {
                  case (sum, i) =>
                      (sum >= (k + 1).U) && io.freeList(i)
              }).asUInt
            )
        }

        for (k <- 0 until nWays) { // 把空闲寄存器分配给重命名后的寄存器
            mapped(k).srcL := Mux(io.src(k).srcL === 0.U, 0.U, allocIndices(k * 3))
            mapped(k).srcR := Mux(io.src(k).srcR === 0.U, 0.U, allocIndices(k * 3 + 1))
            mapped(k).dest := Mux(io.src(k).dest === 0.U, 0.U, allocIndices(k * 3 + 2))
        }
    }.otherwise(
      for (k <- 0 until nWays) {
          mapped(k).srcL := DontCare
          mapped(k).srcR := DontCare
          mapped(k).dest := DontCare
      }
    )

    // 重命名第二阶段：检查RAW冒险，把检查后的结果写入RAT (mapped -> renamed)
    when(!io.pause) {
        io.renamed(0).srcL := mapped(0).srcL
        io.renamed(0).srcR := mapped(0).srcR
        io.renamed(0).dest := mapped(0).dest

        for (k <- 1 until nWays) {
            io.renamed(k).dest := mapped(k).dest
            io.renamed(k).srcL := mapped(k).srcL // 默认值
            io.renamed(k).srcR := mapped(k).srcR // 默认值

            // 从最近的指令开始检查
            for (j <- (k - 1) to 0 by -1) {
                when(io.src(k).srcL === io.src(j).dest) {
                    io.renamed(k).srcL := io.renamed(j).dest
                }
                when(io.src(k).srcR === io.src(j).dest) {
                    io.renamed(k).srcR := io.renamed(j).dest
                }
            }
        }
    }.otherwise(
      for (k <- 0 until nWays) {
          io.renamed(k).srcL := DontCare
          io.renamed(k).srcR := DontCare
          io.renamed(k).dest := DontCare
      }
    )

    when(!io.pause) {
        for (k <- 0 until nWays) {
            io.update(k).mapL.src := io.src(k).srcL
            io.update(k).mapL.psy := io.renamed(k).srcL
            io.update(k).mapR.src := io.src(k).srcR
            io.update(k).mapR.psy := io.renamed(k).srcR
            io.update(k).mapD.src := io.src(k).dest
            io.update(k).mapD.psy := io.renamed(k).dest
        }
        RegUpdateEnable := true.B
    }.otherwise {
        for (k <- 0 until nWays) {
            io.update(k).mapL.src := DontCare
            io.update(k).mapL.psy := DontCare
            io.update(k).mapR.src := DontCare
            io.update(k).mapR.psy := DontCare
            io.update(k).mapD.src := DontCare
            io.update(k).mapD.psy := DontCare
        }
        RegUpdateEnable := false.B
    }

    // 于此同时：进行 WAW 冒险检查
    val RegUpdatedestEnable = Wire(Vec(nWays, Bool()))

    RegUpdatedestEnable(nWays - 1) := true.B

    // For each preceding instruction, check if its dest conflicts with any later instruction
    for (k <- 0 until nWays - 1) {
        // Start with assumption that we can update the dest register
        RegUpdatedestEnable(k) := true.B
        // Check against all later instructions
        for (j <- k + 1 until nWays) {
            // If source destination registers are the same but not register 0, there's a WAW hazard
            when(io.src(k).dest === io.src(j).dest && io.src(k).dest =/= 0.U) {
                RegUpdatedestEnable(k) := false.B
            }
        }
    }

    io.updatedestEnable := RegUpdatedestEnable
    io.updateEnable := RegUpdateEnable
}

class RegisterRenamingOld(nWays: Int, nPsyRegs: Int) extends Module {
    val io = IO(new Bundle {
        val src = Vec(
          nWays,
          new Bundle { // 重命名前的寄存器
              val srcL = Input(UInt(5.W)) // 源寄存器1
              val srcR = Input(UInt(5.W)) // 源寄存器2
              val dest = Input(UInt(5.W)) //  目的寄存器
          }
        )
        val commited = Vec( // 已提交的物理寄存器编号（来自Commit阶段）
          nWays,
          new Bundle {
              val srcL = Input(UInt(log2Ceil(nPsyRegs).W))
              val srcR = Input(UInt(log2Ceil(nPsyRegs).W))
              val dest = Input(UInt(log2Ceil(nPsyRegs).W))
          }
        )
        val commitEnable = Input(Bool()) // 该阶段是否允许提交（来自Commit阶段）
        val recover = Input(Bool()) // 恢复指令

        val renamed = Vec(
          nWays,
          new Bundle { // 重命名后的寄存器
              val srcL = Output(UInt(log2Ceil(nPsyRegs).W))
              val srcR = Output(UInt(log2Ceil(nPsyRegs).W))
              val dest = Output(UInt(log2Ceil(nPsyRegs).W))
          }
        )

        val pause = Output(Bool()) // 暂停流水线
    })
    val rat = Module(new RATold(nWays, nPsyRegs))
    val renamingUnit = Module(new RenamingUnit(nWays, nPsyRegs))

    renamingUnit.io.src := io.src
    rat.io.committed := io.commited
    rat.io.recover := io.recover
    rat.io.commitEnable := io.commitEnable

    renamingUnit.io.freeList := rat.io.freeListout
    renamingUnit.io.prefixSum := rat.io.prefixSum
    renamingUnit.io.pause := rat.io.pause
    rat.io.update := renamingUnit.io.update
    rat.io.updateEnable := renamingUnit.io.updateEnable
    rat.io.updatedestEnable := renamingUnit.io.updatedestEnable

    io.renamed := renamingUnit.io.renamed
    io.pause := rat.io.pause
}

object RegisterRenamingOld extends App {
    ChiselStage.emitSystemVerilogFile(
      new RegisterRenamingOld(4, 128),
      Array("--target-dir", "generated")
    )
}
