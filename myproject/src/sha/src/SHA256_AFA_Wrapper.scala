package sha.src

import chisel3._
import chisel3.util._

class SHA256_AFA_Wrapper extends Module {
   val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SHA256Input()))
    // val triggerLaser = Input(Bool()) // For testing AFA attacks only --- IGNORE ---
    val out = Decoupled(UInt(256.W))
    val faultAlarm = Output(Bool()) 
  })

  val core1 = Module(new SHA256())
  val core2 = Module(new SHA256())

  core1.io.in.valid := io.in.valid
  core1.io.in.bits  := io.in.bits
  core2.io.in.valid := io.in.valid
  core2.io.in.bits  := io.in.bits

  dontTouch(core1.io.out)
  dontTouch(core2.io.out)

  /*
  // For testing purposes, we connect the laser trigger to core1 only to simulate AFA attacks --- IGNORE ---
  core1.io.debug_injectFault := io.triggerLaser 
  core2.io.debug_injectFault := false.B  
  */       
  
  io.in.ready := core1.io.in.ready && core2.io.in.ready

  val bothFinished = core1.io.out.valid && core2.io.out.valid
  val hashesMatch  = core1.io.out.bits === core2.io.out.bits

  io.out.valid := bothFinished && hashesMatch
  io.out.bits  := core1.io.out.bits

  core1.io.out.ready := io.out.ready && bothFinished
  core2.io.out.ready := io.out.ready && bothFinished

  val attackDetected = RegInit(false.B)
  when(bothFinished && !hashesMatch) {
    attackDetected := true.B
  }
  io.faultAlarm := attackDetected
}