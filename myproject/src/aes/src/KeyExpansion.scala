package aes

import chisel3._
import chisel3.util._

class KeyExpansion extends Module {
    val io = IO(new Bundle {
        val key       = Input(UInt(128.W))
        val roundKeys = Output(Vec(11, UInt(128.W)))
    })

    val sBox = VecInit(AESConstants.sBox)
    val rcon = VecInit(AESConstants.rcon)

    def subWord(w: UInt): UInt = {
        Cat(sBox(w(31, 24)), sBox(w(23, 16)), sBox(w(15, 8)), sBox(w(7, 0)))
    }

    val words = Wire(Vec(44, UInt(32.W)))
    words(0) := io.key(127, 96)
    words(1) := io.key(95, 64)
    words(2) := io.key(63, 32)
    words(3) := io.key(31, 0)

    for (i <- 4 until 44) {
        val temp = words(i - 1)
        if (i % 4 == 0) {
            val rot = Cat(temp(23, 0), temp(31, 24))
            words(i) := words(i - 4) ^ subWord(rot) ^ rcon((i / 4) - 1)
        } else {
            words(i) := words(i - 4) ^ temp
        }
    }

    for (i <- 0 until 11) {
        io.roundKeys(i) := Cat(words(i * 4), words(i * 4 + 1), words(i * 4 + 2), words(i * 4 + 3))
    }
}
