package sha.gen

import chisel3.stage.ChiselStage
import sha.src.SHA256_AFA_Wrapper

object SHA256Generator extends App {
  (new ChiselStage).emitVerilog(
    new SHA256_AFA_Wrapper(),
    Array("--target-dir", "gen_files")
  )
}