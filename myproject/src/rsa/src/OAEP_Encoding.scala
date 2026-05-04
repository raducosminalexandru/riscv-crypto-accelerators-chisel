package rsa.src

import chisel3._
import chisel3.util._

class OAEP_Encoding_Input(val k: Int, val hLen: Int) extends Bundle {
  val maxMLen = k - 2 * hLen - 2
  val message = Vec(maxMLen, UInt(8.W))
  val seed = Vec(hLen, UInt(8.W))
  val mLen = UInt(log2Ceil(maxMLen + 1).W)
}

class OAEP_Encoding_Output(val k: Int) extends Bundle {
  val encodedMessage = Vec(k, UInt(8.W))
}

class OAEP_Encoding(val hLen: Int = 32, val k: Int = 256) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new OAEP_Encoding_Input(k, hLen)))
    val out = Decoupled(new OAEP_Encoding_Output(k))
  })

  val lHashWords = VecInit(Seq(
    "he3b0c442".U(32.W), "h98fc1c14".U(32.W), "h9afbf4c8".U(32.W), "h996fb924".U(32.W),
    "h27ae41e4".U(32.W), "h649b934c".U(32.W), "ha495991b".U(32.W), "h7852b855".U(32.W)
  ))

  val maxMLen = k - 2 * hLen - 2
  val dbSize = k - hLen - 1
  val dbNumWords = (dbSize + 3) / 4
  val dbWords = RegInit(VecInit(Seq.fill(dbNumWords)(0.U(32.W))))
  val seedWords = RegInit(VecInit(Seq.fill(8)(0.U(32.W))))
  val maskedSeedWords = RegInit(VecInit(Seq.fill(8)(0.U(32.W))))
  
  val sIdle :: sPad :: sMsg :: sFlush :: sDone :: Nil = Enum(5)
  val state = RegInit(sIdle)
  
  val idxCounter = RegInit(0.U(log2Ceil(maxMLen + 2).W))
  val msgCounter = RegInit(0.U(log2Ceil(maxMLen + 1).W))
  val shiftBuffer = RegInit(0.U(32.W))
  val byteCount = RegInit(0.U(3.W))
  val wordCount = RegInit(0.U(log2Ceil(dbNumWords + 1).W))
  val inReg = Reg(new OAEP_Encoding_Input(k, hLen))
  
  val validMLen = Mux(inReg.mLen > maxMLen.U, maxMLen.U, inReg.mLen)
  val psLength = maxMLen.U - validMLen

  io.in.ready := state === sIdle

  val pushByteValid = WireDefault(false.B)
  val pushByteData = WireDefault(0.U(8.W))

  when(pushByteValid) {
    shiftBuffer := Cat(shiftBuffer(23, 0), pushByteData)
    byteCount := byteCount + 1.U
    when(byteCount === 3.U) {
      dbWords(wordCount) := Cat(shiftBuffer(23, 0), pushByteData)
      wordCount := wordCount + 1.U
      byteCount := 0.U
    }
  }

  switch(state) {
    is(sIdle) {
      when(io.in.fire) {
        inReg := io.in.bits
        idxCounter := 0.U
        msgCounter := 0.U
        
        for (i <- 0 until 8) {
          seedWords(i) := Cat(io.in.bits.seed(i * 4), io.in.bits.seed(i * 4 + 1), io.in.bits.seed(i * 4 + 2), io.in.bits.seed(i * 4 + 3))
        }
        
        for (i <- 0 until 8) {
          dbWords(i) := lHashWords(i)
        }
        
        wordCount := 8.U
        byteCount := 0.U
        shiftBuffer := 0.U
        state := sPad
      }
    }
    
    is(sPad) {
      pushByteValid := true.B
      when(idxCounter < psLength) {
        pushByteData := 0.U(8.W)
        idxCounter := idxCounter + 1.U
      }.elsewhen(idxCounter === psLength) {
        pushByteData := 1.U(8.W)
        idxCounter := idxCounter + 1.U
        state := sMsg
      }
    }
    
    is(sMsg) {
      when(msgCounter < validMLen) {
        pushByteValid := true.B
        pushByteData := inReg.message(msgCounter)
        msgCounter := msgCounter + 1.U
      }.otherwise {
        state := sFlush
      }
    }

    is(sFlush) {
      when(byteCount =/= 0.U) {
        val shiftAmt = (4.U - byteCount) * 8.U
        dbWords(wordCount) := (shiftBuffer << shiftAmt)(31, 0)
      }
      state := sDone
    }
    
    is(sDone) {
      when(io.out.fire) { 
        state := sIdle 
      }
    }
  }

  val sIdle2 :: sFeedSeed :: sFeedDB :: sXOR_DB :: sXOR_Seed :: sDone2 :: Nil = Enum(6)
  val state2 = RegInit(sIdle2)

  val mgf1 = Module(new MGF1())

  val mgfWordIdx = RegInit(0.U(32.W))
  val maskBaseWordIdx = RegInit(0.U(32.W))

  mgf1.io.in.start := false.B
  mgf1.io.in.maskLen := 0.U
  mgf1.io.in.zData.valid := false.B
  mgf1.io.in.zData.bits.data := 0.U
  mgf1.io.in.zData.bits.validBits := 0.U
  mgf1.io.in.zData.bits.isLast := false.B
  mgf1.io.out.hashOut.ready := false.B

  val isFirstCycleFeedDB = RegNext(state2 === sXOR_DB)

  io.out.valid := (state2 === sDone2)

  switch(state2) {
    is(sIdle2) {
      when(state === sDone) {
        mgf1.io.in.start := true.B
        mgf1.io.in.maskLen := dbSize.U
        mgfWordIdx := 0.U
        state2 := sFeedSeed
      }
    }

    is(sFeedSeed) {
      mgf1.io.in.zData.valid := true.B
      mgf1.io.in.zData.bits.data := seedWords(mgfWordIdx)
      mgf1.io.in.zData.bits.validBits := 32.U
      mgf1.io.in.zData.bits.isLast := (mgfWordIdx === 7.U)

      when(mgf1.io.in.zData.ready) {
        mgfWordIdx := mgfWordIdx + 1.U
        when(mgfWordIdx === 7.U) {
          maskBaseWordIdx := 0.U
          state2 := sXOR_DB
        }
      }
    }

    is(sXOR_DB) {
      mgf1.io.out.hashOut.ready := true.B
      when(mgf1.io.out.hashOut.valid) {
        val maskWord256 = mgf1.io.out.hashOut.bits

        for (i <- 0 until 8) {
          val targetWordIdx = maskBaseWordIdx + i.U
          when(targetWordIdx < dbNumWords.U) {
            val chunk = maskWord256(255 - i * 32, 224 - i * 32)
            dbWords(targetWordIdx) := dbWords(targetWordIdx) ^ chunk
          }
        }

        maskBaseWordIdx := maskBaseWordIdx + 8.U
        when(mgf1.io.out.isLastBlock) {
          mgfWordIdx := 0.U
          state2 := sFeedDB
        }
      }
    }

    is(sFeedDB) {
      when(isFirstCycleFeedDB) {
        mgf1.io.in.start := true.B
        mgf1.io.in.maskLen := hLen.U
      }

      mgf1.io.in.zData.valid := !isFirstCycleFeedDB
      
      val bytesRemaining = dbSize.U - (mgfWordIdx * 4.U)
      val validBits = Mux(bytesRemaining >= 4.U, 32.U, bytesRemaining * 8.U)
      val isLastWord = (bytesRemaining <= 4.U)

      val rawWord = dbWords(mgfWordIdx)
      val alignedWord = WireInit(rawWord)
      
      when(bytesRemaining === 3.U) {
        alignedWord := rawWord >> 8.U
      }.elsewhen(bytesRemaining === 2.U) {
        alignedWord := rawWord >> 16.U
      }.elsewhen(bytesRemaining === 1.U) {
        alignedWord := rawWord >> 24.U
      }

      mgf1.io.in.zData.bits.data := alignedWord
      mgf1.io.in.zData.bits.validBits := validBits
      mgf1.io.in.zData.bits.isLast := isLastWord

      when(mgf1.io.in.zData.ready && mgf1.io.in.zData.valid) {
        mgfWordIdx := mgfWordIdx + 1.U
        when(isLastWord) {
          maskBaseWordIdx := 0.U
          state2 := sXOR_Seed
        }
      }
    }

    is(sXOR_Seed) {
      mgf1.io.out.hashOut.ready := true.B
      when(mgf1.io.out.hashOut.valid) {
        val maskWord256 = mgf1.io.out.hashOut.bits

        for (i <- 0 until 8) {
          val chunk = maskWord256(255 - i * 32, 224 - i * 32)
          maskedSeedWords(i) := seedWords(i) ^ chunk
        }

        when(mgf1.io.out.isLastBlock) {
          state2 := sDone2
        }
      }
    }

    is(sDone2) {
      when(io.out.fire) { 
        state2 := sIdle2 
      }
    }
  }
  
  io.out.bits.encodedMessage(0) := 0.U(8.W)
  
  for (i <- 0 until 8) {
    val w = maskedSeedWords(i)
    for (j <- 0 until 4) {
      io.out.bits.encodedMessage(1 + i * 4 + j) := w(31 - j * 8, 24 - j * 8)
    }
  }
  
  for (i <- 0 until dbNumWords) {
    val w = dbWords(i)
    for (j <- 0 until 4) {
      val outIdx = 1 + hLen + i * 4 + j
      if (outIdx < k) {
        io.out.bits.encodedMessage(outIdx) := w(31 - j * 8, 24 - j * 8)
      }
    }
  }
}