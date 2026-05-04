package sha.src

import chisel3._
import chisel3.util._

class SHA256Padder extends Module {
  val io = IO(new SHA256Interface())

  val s_recv :: s_emit_full :: s_append_1 :: s_check_space :: s_emit_overflow :: s_emit_final :: Nil = Enum(6)
  val state = RegInit(s_recv)

  val buffer = RegInit(0.U(512.W))
  val bitsInBuffer = RegInit(0.U(10.W))
  val totalMsgLen = RegInit(0.U(64.W))

  val pendingData = RegInit(0.U(32.W))
  val pendingBits = RegInit(0.U(6.W))
  val pendingIsLast = RegInit(false.B)

  io.out.valid := false.B
  io.out.bits := buffer
  io.lastBlock := false.B

  io.in.ready := (pendingBits === 0.U) && !pendingIsLast && (state === s_recv)

  val maskTable = VecInit(Seq.tabulate(64)(i => ((BigInt(1) << i) - 1).U(64.W)))

when(io.in.valid && io.in.ready) {
    val mask = maskTable(io.in.bits.validBits)
    pendingData := io.in.bits.data & mask
    pendingBits := io.in.bits.validBits
    pendingIsLast := io.in.bits.isLast
    totalMsgLen := totalMsgLen + io.in.bits.validBits
  }

  val spaceLeft = 512.U(10.W) - bitsInBuffer
  val takeBits = Mux(pendingBits > spaceLeft, spaceLeft(5,0), pendingBits)

  switch(state) {
    is(s_recv) {
      when(pendingBits > 0.U) {
        val shiftDown = pendingBits - takeBits
        val extracted = pendingData >> shiftDown
        
        val extMask = maskTable(takeBits)
        val cleanExtracted = extracted & extMask

        val shiftUp = spaceLeft - takeBits
        val ext512 = WireDefault(0.U(512.W))
        ext512 := cleanExtracted
        buffer := buffer | (ext512 << shiftUp)

        bitsInBuffer := bitsInBuffer + takeBits
        pendingBits := pendingBits - takeBits

        when(bitsInBuffer + takeBits === 512.U) {
          state := s_emit_full
        }
      }.elsewhen(pendingIsLast) {
        state := s_append_1
        pendingIsLast := false.B
      }
    }

    is(s_emit_full) {
      io.out.valid := true.B
      when(io.out.ready) {
        buffer := 0.U
        bitsInBuffer := 0.U
        state := s_recv 
      }
    }

    is(s_append_1) {
      val shiftAmt = 512.U(10.W) - bitsInBuffer - 1.U
      val bitOne = WireDefault(1.U(512.W))
      
      buffer := buffer | (bitOne << shiftAmt)
      bitsInBuffer := bitsInBuffer + 1.U
      state := s_check_space
    }

    is(s_check_space) {
      when(bitsInBuffer > 448.U) {
        state := s_emit_overflow
      }.otherwise {
        state := s_emit_final
      }
    }

    is(s_emit_overflow) {
      io.out.valid := true.B
      when(io.out.ready) {
        buffer := 0.U 
        bitsInBuffer := 0.U
        state := s_emit_final
      }
    }

    is(s_emit_final) {
      val lenExt = WireDefault(0.U(512.W))
      lenExt := totalMsgLen

      io.out.valid := true.B
      io.out.bits := buffer | lenExt 
      io.lastBlock := true.B
      
      when(io.out.ready) {
        buffer := 0.U
        bitsInBuffer := 0.U
        totalMsgLen := 0.U
        state := s_recv
      }
    }
  }
}