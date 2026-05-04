package aes

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR

class AESInput extends Bundle {
    val plaintext = UInt(128.W)
    val key       = UInt(128.W)
    val isDecrypt = Bool()
}

class AESInterface extends Bundle {
    val in       = Flipped(Decoupled(new AESInput))
    val out      = Decoupled(UInt(128.W))
    val dummyOut = Output(Bool())
}

class AES128 extends Module {
    val io = IO(new AESInterface)

    val sIdle :: sCompute :: sDone :: Nil = Enum(3)
    val stateReg     = RegInit(sIdle)

    val dataReg      = RegInit(0.U(128.W))
    val isDecryptReg = RegInit(false.B)
    val roundCounter = RegInit(0.U(4.W))

    val keyReg      = RegInit(0.U(128.W))
    val keyExpander = Module(new KeyExpansion)
    keyExpander.io.key := Mux(stateReg === sIdle, io.in.bits.key, keyReg)
    val roundKeys   = keyExpander.io.roundKeys

    val noiseReg = RegInit(0.U(128.W))
    val randomNoise = Cat(LFSR(32), LFSR(32), LFSR(32), LFSR(32))

    def uintToMatrix(in: UInt): Vec[Vec[UInt]] = {
        val m = Wire(Vec(4, Vec(4, UInt(8.W))))
        for (i <- 0 until 16) {
            val c = i / 4
            val r = i % 4
            m(c)(r) := in(127 - (i * 8), 120 - (i * 8))
        }
        m
    }

    def matrixToUint(m: Vec[Vec[UInt]]): UInt = {
        val flat = for (c <- 0 until 4; r <- 0 until 4) yield m(c)(r)
        Cat(flat)
    }

    def byteSub(in: UInt, useInverse: Boolean): UInt = {
        val rom = VecInit(if (useInverse) AESConstants.invSBox else AESConstants.sBox)
        val bytes = for (i <- 0 until 16) yield rom(in(127 - (i * 8), 120 - (i * 8)))
        Cat(bytes)
    }

    def shiftRow(in: UInt): UInt = {
        val m = uintToMatrix(in)
        val next = Wire(Vec(4, Vec(4, UInt(8.W))))
        for (r <- 0 until 4; c <- 0 until 4) {
            next(c)(r) := m((c + r) % 4)(r)
        }
        matrixToUint(next)
    }

    def invShiftRow(in: UInt): UInt = {
        val m = uintToMatrix(in)
        val next = Wire(Vec(4, Vec(4, UInt(8.W))))
        for (r <- 0 until 4; c <- 0 until 4) {
            next(c)(r) := m((c - r + 4) % 4)(r)
        }
        matrixToUint(next)
    }

    def multiplyBy2(b: UInt): UInt = {
        val s = (b << 1)(7, 0)
        Mux(b(7), s ^ 0x1B.U(8.W), s)
    }

    def gfMul(b: UInt, factor: Int): UInt = {
        val x2 = multiplyBy2(b)
        val x4 = multiplyBy2(x2)
        val x8 = multiplyBy2(x4)
        factor match {
            case 2  => x2
            case 3  => x2 ^ b
            case 9  => x8 ^ b
            case 11 => x8 ^ x2 ^ b
            case 13 => x8 ^ x4 ^ b
            case 14 => x8 ^ x4 ^ x2
            case _  => b
        }
    }

    def mixColumn(in: UInt): UInt = {
        val m = uintToMatrix(in)
        val next = Wire(Vec(4, Vec(4, UInt(8.W))))
        for (c <- 0 until 4) {
            val d = m(c)
            next(c)(0) := gfMul(d(0), 2) ^ gfMul(d(1), 3) ^ d(2) ^ d(3)
            next(c)(1) := d(0) ^ gfMul(d(1), 2) ^ gfMul(d(2), 3) ^ d(3)
            next(c)(2) := d(0) ^ d(1) ^ gfMul(d(2), 2) ^ gfMul(d(3), 3)
            next(c)(3) := gfMul(d(0), 3) ^ d(1) ^ d(2) ^ gfMul(d(3), 2)
        }
        matrixToUint(next)
    }

    def invMixColumn(in: UInt): UInt = {
        val m = uintToMatrix(in)
        val next = Wire(Vec(4, Vec(4, UInt(8.W))))
        for (c <- 0 until 4) {
            val d = m(c)
            next(c)(0) := gfMul(d(0), 14) ^ gfMul(d(1), 11) ^ gfMul(d(2), 13) ^ gfMul(d(3), 9)
            next(c)(1) := gfMul(d(0), 9)  ^ gfMul(d(1), 14) ^ gfMul(d(2), 11) ^ gfMul(d(3), 13)
            next(c)(2) := gfMul(d(0), 13) ^ gfMul(d(1), 9)  ^ gfMul(d(2), 14) ^ gfMul(d(3), 11)
            next(c)(3) := gfMul(d(0), 11) ^ gfMul(d(1), 13) ^ gfMul(d(2), 9)  ^ gfMul(d(3), 14)
        }
        matrixToUint(next)
    }

    io.in.ready  := (stateReg === sIdle)
    io.out.valid := (stateReg === sDone)
    io.out.bits  := dataReg
    io.dummyOut  := noiseReg.xorR

    noiseReg := noiseReg ^ randomNoise ^ (noiseReg << 3.U) ^ (noiseReg >> 5.U)

    switch(stateReg) {
        is(sIdle) {
            when(io.in.valid) {
                keyReg       := io.in.bits.key
                isDecryptReg := io.in.bits.isDecrypt
                roundCounter := 0.U
                
                val initKey = Mux(io.in.bits.isDecrypt, roundKeys(10), roundKeys(0))
                dataReg      := io.in.bits.plaintext ^ initKey
                stateReg     := sCompute
            }
        }

        is(sCompute) {
            val nextRound = roundCounter + 1.U
            roundCounter := nextRound

            val currentKey = roundKeys(Mux(isDecryptReg, 10.U - nextRound, nextRound))

            when(!isDecryptReg) {
                val sub       = byteSub(dataReg, false)
                val shift     = shiftRow(sub)
                val nextState = Mux(nextRound < 10.U, mixColumn(shift) ^ currentKey, shift ^ currentKey)
                dataReg := nextState

                when(nextRound === 10.U) { stateReg := sDone }
            } .otherwise {
                val invShift  = invShiftRow(dataReg)
                val invSub    = byteSub(invShift, true)
                val addKey    = invSub ^ currentKey
                val nextState = Mux(nextRound < 10.U, invMixColumn(addKey), addKey)
                dataReg := nextState

                when(nextRound === 10.U) { stateReg := sDone }
            }
        }

        is(sDone) {
            when(io.out.ready) {
                stateReg := sIdle
            }
        }
    }
}