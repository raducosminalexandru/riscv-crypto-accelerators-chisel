package sha

import chisel3.stage.ChiselStage

object SHA256Generator extends App {
  (new ChiselStage).emitVerilog(
    new SHA256(),
    Array("--target-dir", "gen")
  )
}