package rsa.gen

import chisel3.stage.ChiselStage
import rsa.src.MGF1

object MGFGenerator extends App {
    val k = 256
    val hLen = 32
  (new ChiselStage).emitVerilog(
    new MGF1(64),
    Array("--target-dir", "gen_files")
  )
}
