package rsa.gen

import chisel3.stage.ChiselStage
import rsa.src.OAEP_Encoding

object OAEP_Encoding_Generator extends App {
    val k = 256
    val hLen = 32
  (new ChiselStage).emitVerilog(
    new OAEP_Encoding(hLen, k),
    Array("--target-dir", "gen_files")
  )
}