package aes.gen
import chisel3.stage.ChiselStage
import aes.AES128

object AES128Generator extends App {
  (new ChiselStage).emitVerilog(
    new AES128(),
    Array("--target-dir", "gen_files")
  )
}
