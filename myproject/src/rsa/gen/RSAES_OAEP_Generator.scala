package rsa.gen

import chisel3.stage.ChiselStage

object RSAES_OAEP_Generator extends App {
    val keyBits = 1024
  (new ChiselStage).emitVerilog(
    new rsa.src.RSAES_OAEP(keyBits),
    Array("--target-dir", "gen_files")
  )
}
