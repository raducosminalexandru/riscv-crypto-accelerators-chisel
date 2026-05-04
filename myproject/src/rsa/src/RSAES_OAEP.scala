package rsa.src

import chisel3._
import chisel3.util._

class RSAES_OAEP_Input(val keyBits: Int) extends Bundle {
  val isDecrypt = Bool()         
  val n         = UInt(keyBits.W) 
  val exp       = UInt(keyBits.W) 
  val dataIn    = UInt(keyBits.W) 
  val dataLen   = UInt(log2Ceil((keyBits / 8) + 1).W) 
  val r2        = UInt(keyBits.W)
  val seed      = Vec(32, UInt(8.W)) 
}

class RSAES_OAEP_Output(val keyBits: Int) extends Bundle {
  val c= UInt(keyBits.W) 
  val mLen  = UInt(log2Ceil((keyBits / 8) + 1).W) 
  val isError = Bool()          
}

class RSAES_OAEP(val keyBits: Int = 2048) extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new RSAES_OAEP_Input(keyBits)))
    val out = Decoupled(new RSAES_OAEP_Output(keyBits))
  })

  val sIdle :: sEncode :: sModExp :: sDecode :: sDone :: Nil = Enum(5)
  val state = RegInit(sIdle)

  val hLenBytes = 32 
  val kBytes = keyBits / 8
  val oaepEncoder = Module(new OAEP_Encoding(hLen = hLenBytes, k = kBytes))
  val oaepDecoder = Module(new OAEP_Decoding(hLen = hLenBytes, k = kBytes))

  val nReg = Reg(UInt(keyBits.W))
  val expReg = Reg(UInt(keyBits.W))
  val mReg = Reg(UInt(keyBits.W))
  val dataLenReg = Reg(UInt(log2Ceil(kBytes + 1).W))
  val seedReg = Reg(Vec(hLenBytes, UInt(8.W)))

  val encodedMsgReg = Reg(UInt(keyBits.W)) 
  val encoderFed = RegInit(false.B)

  val maxDataLen = kBytes - 2 * hLenBytes - 2
  oaepEncoder.io.in.valid := false.B
  oaepEncoder.io.in.bits.mLen := dataLenReg
  
  for (i <- 0 until maxDataLen) {
    oaepEncoder.io.in.bits.message(i) := mReg(i * 8 + 7, i * 8)
  }

  for (i <- 0 until hLenBytes) {
    oaepEncoder.io.in.bits.seed(i) := seedReg(i)
  }

  oaepEncoder.io.out.ready := false.B

  io.in.ready := (state === sIdle)
  io.out.valid := (state === sDone)
  io.out.bits.c := 0.U
  io.out.bits.mLen := 0.U
  io.out.bits.isError := false.B

  val r2Reg = Reg(UInt(keyBits.W))
  val isDecryptReg = RegInit(false.B)
  val modExpResReg = Reg(UInt(keyBits.W)) 
  val modExpFed = RegInit(false.B)
  val modExpModule = Module(new ModExp(keyBits))

  modExpModule.io.in.valid := false.B
  modExpModule.io.in.bits.base := 0.U
  modExpModule.io.in.bits.exp := 0.U
  modExpModule.io.in.bits.n := 0.U
  modExpModule.io.in.bits.r2 := 0.U
  modExpModule.io.out.ready := false.B

  val decoderFed = RegInit(false.B)
  val decodedMsgReg = Reg(UInt(keyBits.W))
  val decodedMLenReg = Reg(UInt(log2Ceil(kBytes + 1).W))
  val decodeErrorReg = RegInit(false.B)
  oaepDecoder.io.in.valid := false.B
  for (i <- 0 until kBytes) {
    oaepDecoder.io.in.bits.encodedMessage(i) := 0.U(8.W)
  }
  oaepDecoder.io.out.ready := false.B

  switch(state) {
    is(sIdle) {
      encoderFed := false.B
      modExpFed := false.B
      decoderFed := false.B
      when(io.in.fire) {
        nReg := io.in.bits.n
        expReg := io.in.bits.exp
        mReg := io.in.bits.dataIn
        dataLenReg := io.in.bits.dataLen
        r2Reg := io.in.bits.r2               
        isDecryptReg := io.in.bits.isDecrypt 
        seedReg := io.in.bits.seed

        when(io.in.bits.isDecrypt) {
          state := sModExp
        } .otherwise {
          state := sEncode
        }
      }
    }

    is(sEncode) {
      oaepEncoder.io.in.valid := !encoderFed
      when(oaepEncoder.io.in.fire) {
        encoderFed := true.B
      }

      oaepEncoder.io.out.ready := true.B
      when(oaepEncoder.io.out.fire) {
        encodedMsgReg := Cat(oaepEncoder.io.out.bits.encodedMessage)
        state := sModExp
        encoderFed := false.B 
      }
    }

    is(sModExp) {
      modExpModule.io.in.valid := !modExpFed
      modExpModule.io.in.bits.base := Mux(isDecryptReg, mReg, encodedMsgReg)
      modExpModule.io.in.bits.exp  := expReg
      modExpModule.io.in.bits.n    := nReg
      modExpModule.io.in.bits.r2   := r2Reg

      when(modExpModule.io.in.fire) {
        modExpFed := true.B
      }

      modExpModule.io.out.ready := true.B
      when(modExpModule.io.out.fire) {
        modExpResReg := modExpModule.io.out.bits
        modExpFed := false.B 

        when(isDecryptReg) {
          state := sDecode
        } .otherwise {
          io.out.bits.c := modExpModule.io.out.bits
          state := sDone   
        }
      }
    }

    is(sDecode) {
      oaepDecoder.io.in.valid := !decoderFed
      
      for (i <- 0 until kBytes) {
        oaepDecoder.io.in.bits.encodedMessage(i) := modExpResReg((kBytes - 1 - i) * 8 + 7, (kBytes - 1 - i) * 8)
      }

      when(oaepDecoder.io.in.fire) {
        decoderFed := true.B
      }

      oaepDecoder.io.out.ready := true.B
      when(oaepDecoder.io.out.fire) {
        decodedMsgReg  := Cat(oaepDecoder.io.out.bits.message.reverse)
        decodedMLenReg := oaepDecoder.io.out.bits.mLen
        decodeErrorReg := oaepDecoder.io.out.bits.isError

        decoderFed := false.B
        state := sDone
      }
    }

    is(sDone) {
      io.out.bits.c := Mux(isDecryptReg, decodedMsgReg, modExpResReg)
      io.out.bits.mLen := Mux(isDecryptReg, decodedMLenReg, 0.U)
      io.out.bits.isError := Mux(isDecryptReg, decodeErrorReg, false.B)
      
      when(io.out.fire) {
        state := sIdle
      }
    }
  }
}