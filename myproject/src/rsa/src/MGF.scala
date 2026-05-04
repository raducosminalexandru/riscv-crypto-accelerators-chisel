package rsa.src

import chisel3._
import chisel3.util._
import sha.src._

class MGF1Input extends Bundle {
  val zData = Flipped(Decoupled(new SHA256Input()))
  val maskLen = Input(UInt(32.W))
  val start = Input(Bool())
}

class MGF1Output extends Bundle {
  val hashOut = Decoupled(UInt(256.W))
  val validBytes = Output(UInt(6.W))
  val isLastBlock = Output(Bool())
}

class MGF1(maxZWords: Int = 64) extends Module {
  val io = IO(new Bundle {
    val in = new MGF1Input()
    val out = new MGF1Output()
  })

  val sha256 = Module(new SHA256())

  val s_idle :: s_store_z :: s_feed_z :: s_feed_c :: s_wait_hash :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val zDataRegs = Reg(Vec(maxZWords, UInt(32.W)))
  val zBitsRegs = Reg(Vec(maxZWords, UInt(6.W)))
  val zWordCount = RegInit(0.U(log2Ceil(maxZWords + 1).W))
  val zIndex = RegInit(0.U(log2Ceil(maxZWords + 1).W))

  val counter = RegInit(0.U(32.W))
  val bytesRemaining = RegInit(0.U(32.W))

  io.in.zData.ready := false.B
  
  sha256.io.in.valid := false.B
  sha256.io.in.bits.data := 0.U
  sha256.io.in.bits.validBits := 0.U
  sha256.io.in.bits.isLast := false.B
  sha256.io.out.ready := false.B

  io.out.hashOut.valid := false.B
  io.out.hashOut.bits := sha256.io.out.bits
  io.out.validBytes := 0.U
  io.out.isLastBlock := false.B

  switch(state) {
    is(s_idle) {
      when(io.in.start) {
        bytesRemaining := io.in.maskLen
        counter := 0.U
        zWordCount := 0.U
        state := s_store_z
      }
    }

    is(s_store_z) {
      io.in.zData.ready := true.B
      when(io.in.zData.valid) {
        zDataRegs(zWordCount) := io.in.zData.bits.data
        zBitsRegs(zWordCount) := io.in.zData.bits.validBits
        zWordCount := zWordCount + 1.U
        when(io.in.zData.bits.isLast) {
          zIndex := 0.U
          state := s_feed_z
        }
      }
    }

    is(s_feed_z) {
      sha256.io.in.valid := true.B
      sha256.io.in.bits.data := zDataRegs(zIndex)
      sha256.io.in.bits.validBits := zBitsRegs(zIndex)
      sha256.io.in.bits.isLast := false.B

      when(sha256.io.in.ready) {
        zIndex := zIndex + 1.U
        when(zIndex === zWordCount - 1.U) {
          state := s_feed_c
        }
      }
    }

    is(s_feed_c) {
      sha256.io.in.valid := true.B
      sha256.io.in.bits.data := counter
      sha256.io.in.bits.validBits := 32.U
      sha256.io.in.bits.isLast := true.B

      when(sha256.io.in.ready) {
        state := s_wait_hash
      }
    }

    is(s_wait_hash) {
      sha256.io.out.ready := io.out.hashOut.ready
      io.out.hashOut.valid := sha256.io.out.valid
      
      val bytesThisHash = Mux(bytesRemaining >= 32.U, 32.U, bytesRemaining(5, 0))
      io.out.validBytes := bytesThisHash
      
      val isLastMaskBlock = (bytesRemaining <= 32.U)
      io.out.isLastBlock := isLastMaskBlock

      when(sha256.io.out.valid && io.out.hashOut.ready) {
        when(isLastMaskBlock) {
          state := s_idle
        }.otherwise {
          bytesRemaining := bytesRemaining - 32.U
          counter := counter + 1.U
          zIndex := 0.U
          state := s_feed_z
        }
      }
    }
  }
}