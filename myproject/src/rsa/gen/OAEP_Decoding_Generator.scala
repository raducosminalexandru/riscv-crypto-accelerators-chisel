package rsa.gen

import chisel3.stage.ChiselStage
import rsa.src.OAEP_Decoding

object OAEP_Decoding_Generator extends App {
    val k = 256
    val hLen = 32
  (new ChiselStage).emitVerilog(
    new OAEP_Decoding(hLen, k),
    Array("--target-dir", "gen_files")
  )
}