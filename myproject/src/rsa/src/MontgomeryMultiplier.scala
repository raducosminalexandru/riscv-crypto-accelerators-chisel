package rsa.src

import chisel3._
import chisel3.util._

class MontgomeryMultiplier(val keyBits: Int = 2048) extends Module {
  val wordSize = 64
  val numWords = (keyBits + wordSize - 1) / wordSize

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val a = UInt(keyBits.W)
      val b = UInt(keyBits.W)
      val m = UInt(keyBits.W)
    }))
    val out = Decoupled(UInt(keyBits.W))
  })

  val sIdle :: sInitInner :: sAddInner :: sShift :: sFinal :: Nil = Enum(5)
  val state = RegInit(sIdle)

  val sReg = Reg(Vec(numWords, UInt(wordSize.W)))
  val bReg = Reg(Vec(numWords, UInt(wordSize.W)))
  val mReg = Reg(Vec(numWords, UInt(wordSize.W)))
  val aReg = Reg(UInt(keyBits.W))
  
  val sumReg = Reg(Vec(numWords, UInt(wordSize.W)))

  val count = Reg(UInt(log2Ceil(keyBits + 1).W))
  val wordCounter = Reg(UInt(log2Ceil(numWords + 1).W))
  val carryReg = Reg(UInt(2.W))

  val sExtra = RegInit(0.U(1.W))

  val a_i = Reg(UInt(1.W))
  val q_i = Reg(UInt(1.W))

  io.in.ready := (state === sIdle)
  io.out.valid := (state === sFinal)

  val sRegCombined = Cat(sExtra, sReg.asUInt)
  val mRegCombined = mReg.asUInt
  val sIsGreaterOrEqual = sRegCombined >= mRegCombined
  val finalResult = Mux(sIsGreaterOrEqual, sRegCombined - mRegCombined, sRegCombined)
  
  io.out.bits := finalResult(keyBits - 1, 0)

  switch(state) {
    is(sIdle) {
      when(io.in.fire) {
        aReg := io.in.bits.a
        
        // 1. Create zero-padded versions of b and m
        val totalWidth = numWords * wordSize
        val paddedB = io.in.bits.b.pad(totalWidth)
        val paddedM = io.in.bits.m.pad(totalWidth)

        for (i <- 0 until numWords) {
          // 2. Slice from the padded versions instead of directly from io.in.bits
          bReg(i) := paddedB((i + 1) * wordSize - 1, i * wordSize)
          mReg(i) := paddedM((i + 1) * wordSize - 1, i * wordSize)
          sReg(i) := 0.U
        }
        sExtra := 0.U 
        
        count := 0.U
        state := sInitInner
      }
    }

    is(sInitInner) {
      val currentA = aReg(0)
      a_i := currentA
      
      val addB0 = Mux(currentA === 1.U, bReg(0), 0.U(wordSize.W))
      val temp1_0 = sReg(0) + addB0
      val currentQ = temp1_0(0)
      q_i := currentQ

      wordCounter := 0.U
      carryReg := 0.U
      state := sAddInner
    }

    is(sAddInner) {
      val sWord = sReg(wordCounter)
      val bWord = Mux(a_i === 1.U, bReg(wordCounter), 0.U(wordSize.W))
      val mWord = Mux(q_i === 1.U, mReg(wordCounter), 0.U(wordSize.W))

      val totalSum = sWord +& bWord +& mWord + carryReg
      
      sumReg(wordCounter) := totalSum(wordSize - 1, 0)
      carryReg := totalSum(wordSize + 1, wordSize)

      wordCounter := wordCounter + 1.U
      when(wordCounter === (numWords - 1).U) {
        state := sShift
      }
    }

    is(sShift) {
      val topSum = sExtra + carryReg

      for (i <- 0 until numWords - 1) {
        sReg(i) := Cat(sumReg(i + 1)(0), sumReg(i)(wordSize - 1, 1))
      }
      sReg(numWords - 1) := Cat(topSum(0), sumReg(numWords - 1)(wordSize - 1, 1))
      sExtra := topSum(1)

      aReg := aReg >> 1
      count := count + 1.U

      when(count === (keyBits - 1).U) {
        state := sFinal
      } .otherwise {
        state := sInitInner
      }
    }

    is(sFinal) {
      when(io.out.fire) {
        state := sIdle
      }
    }
  }
}