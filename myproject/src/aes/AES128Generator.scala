package aes
import chisel3.stage.ChiselStage

object AES128Generator extends App {
  (new ChiselStage).emitVerilog(
    new AES128(),
    Array("--target-dir", "gen")
  )
}
