package rsa.src

import chisel3._
import chisel3.util._


class OAEP_Decoding_Input(val k: Int, val hLen: Int) extends Bundle {
  val encodedMessage = Vec(k, UInt(8.W))
}

class OAEP_Decoding_Output(val k: Int, val hLen: Int) extends Bundle {
  val message = Vec(k - 2 * hLen - 2, UInt(8.W))
  val mLen    = UInt(log2Ceil(k - 2 * hLen - 1).W)
  val isError = Bool()
}

class OAEP_Decoding(val hLen: Int = 32, val k: Int = 256) extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new OAEP_Decoding_Input(k, hLen)))
    val out = Decoupled(new OAEP_Decoding_Output(k, hLen))
  })

  val lHashWords = VecInit(Seq(
    "he3b0c442".U(32.W), "h98fc1c14".U(32.W),
    "h9afbf4c8".U(32.W), "h996fb924".U(32.W),
    "h27ae41e4".U(32.W), "h649b934c".U(32.W),
    "ha495991b".U(32.W), "h7852b855".U(32.W)
  ))

  val dbSize     = k - hLen - 1
  val maxMLen    = k - 2 * hLen - 2
  val dbNumWords = (dbSize + 3) / 4

  val sIdle :: sFeedMaskedDB :: sXOR_Seed :: sFeedSeed :: sXOR_DB :: sValidate :: sShiftData ::sDone :: Nil = Enum(8)

  val state = RegInit(sIdle)

  val mgf1 = Module(new MGF1())

  val byte0Reg = RegInit(0.U(8.W))

  val maskedSeedWords = RegInit(VecInit(Seq.fill(8)(0.U(32.W))))
  val maskedDBWords   = RegInit(VecInit(Seq.fill(dbNumWords)(0.U(32.W))))
  val seedWords       = RegInit(VecInit(Seq.fill(8)(0.U(32.W))))
  val dbWords         = RegInit(VecInit(Seq.fill(dbNumWords)(0.U(32.W))))

  val mgfWordIdx      = RegInit(0.U(32.W))
  val maskBaseWordIdx = RegInit(0.U(32.W))

  val isValidReg  = RegInit(false.B)
  val mLenReg     = RegInit(0.U(log2Ceil(maxMLen + 1).W))
  val msgStartReg = RegInit(0.U(log2Ceil(dbSize + 1).W))

  val shiftReg = RegInit(0.U((dbSize * 8).W))
  val shiftCount = RegInit(0.U(log2Ceil(dbSize + 1).W))

  mgf1.io.in.start                 := false.B
  mgf1.io.in.maskLen               := 0.U
  mgf1.io.in.zData.valid           := false.B
  mgf1.io.in.zData.bits.data       := 0.U
  mgf1.io.in.zData.bits.validBits  := 0.U
  mgf1.io.in.zData.bits.isLast     := false.B
  mgf1.io.out.hashOut.ready        := false.B

  val isFirstCycleFeedSeed = RegNext(state === sXOR_Seed)

  io.out.bits.isError := !isValidReg
  io.in.ready         := (state === sIdle)
  io.out.valid        := (state === sDone)
  io.out.bits.mLen    := Mux(isValidReg, mLenReg, 0.U)

  val dbBytes = Wire(Vec(dbSize, UInt(8.W)))
  for (i <- 0 until dbNumWords) {
    val w = dbWords(i)
    for (j <- 0 until 4) {
      val byteIdx = i * 4 + j
      if (byteIdx < dbSize) {
        dbBytes(byteIdx) := w(31 - j * 8, 24 - j * 8)
      }
    }
  }

  val dbWide = Cat(dbBytes.reverse)

  for (i <- 0 until maxMLen) {
    val extractedByte = shiftReg(i * 8 + 7, i * 8)
    io.out.bits.message(i) := Mux(isValidReg && (i.U < mLenReg), extractedByte, 0.U(8.W))
  }

  switch(state) {

    is(sIdle) {
      when(io.in.fire) {
        byte0Reg := io.in.bits.encodedMessage(0)

        for (i <- 0 until (hLen / 4)) {
          maskedSeedWords(i) := Cat(
            io.in.bits.encodedMessage(1 + i * 4),
            io.in.bits.encodedMessage(1 + i * 4 + 1),
            io.in.bits.encodedMessage(1 + i * 4 + 2),
            io.in.bits.encodedMessage(1 + i * 4 + 3)
          )
        }

        val dbStartIndex = 1 + hLen
        for (i <- 0 until dbNumWords) {
          val byte0Idx = 33 + i * 4
          val byte1Idx = 33 + i * 4 + 1
          val byte2Idx = 33 + i * 4 + 2
          val byte3Idx = 33 + i * 4 + 3

          val b0 = if (byte0Idx < k) io.in.bits.encodedMessage(byte0Idx) else 0.U(8.W)
          val b1 = if (byte1Idx < k) io.in.bits.encodedMessage(byte1Idx) else 0.U(8.W)
          val b2 = if (byte2Idx < k) io.in.bits.encodedMessage(byte2Idx) else 0.U(8.W)
          val b3 = if (byte3Idx < k) io.in.bits.encodedMessage(byte3Idx) else 0.U(8.W)

          maskedDBWords(i) := Cat(b0, b1, b2, b3)
        }

        mgf1.io.in.start   := true.B
        mgf1.io.in.maskLen := hLen.U

        mgfWordIdx := 0.U
        state      := sFeedMaskedDB
      }
    }

    is(sFeedMaskedDB) {
      mgf1.io.in.zData.valid := true.B

      val bytesRemaining = dbSize.U - (mgfWordIdx * 4.U)
      val validBits      = Mux(bytesRemaining >= 4.U, 32.U, bytesRemaining * 8.U)
      val isLastWord     = (bytesRemaining <= 4.U)

      val rawWord     = maskedDBWords(mgfWordIdx)
      val alignedWord = WireInit(rawWord)

      when(bytesRemaining === 3.U)      { alignedWord := rawWord >> 8.U }
        .elsewhen(bytesRemaining === 2.U) { alignedWord := rawWord >> 16.U }
        .elsewhen(bytesRemaining === 1.U) { alignedWord := rawWord >> 24.U }

      mgf1.io.in.zData.bits.data      := alignedWord
      mgf1.io.in.zData.bits.validBits := validBits
      mgf1.io.in.zData.bits.isLast    := isLastWord

      when(mgf1.io.in.zData.ready) {
        mgfWordIdx := mgfWordIdx + 1.U
        when(isLastWord) {
          maskBaseWordIdx := 0.U
          state           := sXOR_Seed
        }
      }
    }

    is(sXOR_Seed) {
      mgf1.io.out.hashOut.ready := true.B

      when(mgf1.io.out.hashOut.valid) {
        val maskWord256 = mgf1.io.out.hashOut.bits

        for (i <- 0 until 8) {
          val chunk = maskWord256(255 - i * 32, 224 - i * 32)
          seedWords(i) := maskedSeedWords(i) ^ chunk
        }

        when(mgf1.io.out.isLastBlock) {
          mgfWordIdx := 0.U
          state      := sFeedSeed
        }
      }
    }

    is(sFeedSeed) {
      when(isFirstCycleFeedSeed) {
        mgf1.io.in.start   := true.B
        mgf1.io.in.maskLen := dbSize.U
      }

      mgf1.io.in.zData.valid           := !isFirstCycleFeedSeed
      mgf1.io.in.zData.bits.data       := seedWords(mgfWordIdx)
      mgf1.io.in.zData.bits.validBits  := 32.U
      mgf1.io.in.zData.bits.isLast     := (mgfWordIdx === 7.U)

      when(mgf1.io.in.zData.ready && mgf1.io.in.zData.valid) {
        mgfWordIdx := mgfWordIdx + 1.U
        when(mgfWordIdx === 7.U) {
          maskBaseWordIdx := 0.U
          state           := sXOR_DB
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
            dbWords(targetWordIdx) := maskedDBWords(targetWordIdx) ^ chunk
          }
        }

        maskBaseWordIdx := maskBaseWordIdx + 8.U

        when(mgf1.io.out.isLastBlock) {
          state := sValidate
        }
      }
    }

    is(sValidate) {
      val isLHashMatch = WireInit(true.B)
      for (i <- 0 until 8) {
        when(dbWords(i) =/= lHashWords(i)) {
          isLHashMatch := false.B
        }
      }

      val oneFlags  = Wire(Vec(dbSize, Bool()))
      val zeroFlags = Wire(Vec(dbSize, Bool()))

      for (i <- 0 until dbSize) {
        oneFlags(i)  := (dbBytes(i) === 1.U)
        zeroFlags(i) := (dbBytes(i) === 0.U)
      }

      val validOneMask = Wire(Vec(dbSize, Bool()))
      for (i <- 0 until dbSize) {
        validOneMask(i) := oneFlags(i) && (i >= hLen).B
      }

      val firstOneIdx = PriorityEncoder(validOneMask)
      val hasOne      = validOneMask.reduce(_ || _)

      val psValid = WireInit(true.B)
      for (i <- 0 until dbSize) {
        when((i >= hLen).B && (i.U < firstOneIdx)) {
          when(!zeroFlags(i)) { psValid := false.B }
        }
      }

      val valid0 = (byte0Reg === 0.U)

      isValidReg  := isLHashMatch && valid0 && hasOne && psValid
      mLenReg     := Mux(hasOne && (dbSize.U > firstOneIdx),
                        dbSize.U - firstOneIdx - 1.U,
                        0.U)
      msgStartReg := firstOneIdx + 1.U

      shiftReg   := dbWide
      shiftCount := firstOneIdx + 1.U 
      
      state := sShiftData
    }

    is(sShiftData) {
      when(shiftCount > 0.U) {
        shiftReg   := shiftReg >> 8.U 
        shiftCount := shiftCount - 1.U
      } .otherwise {
        state := sDone
      }
    }

    is(sDone) {
      when(io.out.fire) {
        state := sIdle
      }
    }
  }
}