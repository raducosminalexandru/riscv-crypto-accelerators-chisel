package rsa

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import rsa.src.MontgomeryMultiplier

class MontgomeryMultiplierTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MontgomeryMultiplier"

  def montgomeryMultiply(a: BigInt, b: BigInt, m: BigInt, keyBits: Int): BigInt = {
    var res = BigInt(0)
    for (i <- 0 until keyBits) {
      val a_i = a.testBit(i)
      val addB = if (a_i) b else BigInt(0)
      val temp = res + addB
      val q_i = temp.testBit(0)
      val addM = if (q_i) m else BigInt(0)
      
      res = (res + addB + addM) >> 1
    }
    if (res >= m) res - m else res
  }

  it should "compute Montgomery multiplication correctly for 64-bit inputs: example 1" in {
    val keyBits = 64
    test(new MontgomeryMultiplier(keyBits)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      val a = BigInt("1234567890ABCDEF", 16)
      val b = BigInt("FEDCBA0987654321", 16)
      val m = BigInt("13579BDF2468ACE3", 16) 
      
      val expected = montgomeryMultiply(a, b, m, keyBits)

      dut.io.out.ready.poke(true.B)

      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step(1)
      }

      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.a.poke(a.U)
      dut.io.in.bits.b.poke(b.U)
      dut.io.in.bits.m.poke(m.U)

      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)

      var timeout = 0
      while (!dut.io.out.valid.peek().litToBoolean) {
        dut.clock.step(1)
        timeout += 1
        if (timeout > (keyBits * 5)) {
          throw new Exception("Montgomery Multiplier Wait Timed out!")
        }
      }

      val actualOut = dut.io.out.bits.peek().litValue
      assert(actualOut == expected, f"Expected 0x$expected%x, got 0x$actualOut%x")

      dut.clock.step(1)
    }
  }

  it should "compute Montgomery multiplication correctly for 128-bit inputs: example 2" in {
    val keyBits = 128
    test(new MontgomeryMultiplier(keyBits)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      
      val a = BigInt("1234567890ABCDEF1234567890ABCDEF", 16)
      val b = BigInt("FEDCBA0987654321FEDCBA0987654321", 16)
      val m = BigInt("7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16) 

      val expected = montgomeryMultiply(a, b, m, keyBits)

      dut.io.out.ready.poke(true.B)

      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step(1)
      }

      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.a.poke(a.U)
      dut.io.in.bits.b.poke(b.U)
      dut.io.in.bits.m.poke(m.U)

      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)

      var timeout = 0
      while (!dut.io.out.valid.peek().litToBoolean) {
        dut.clock.step(1)
        timeout += 1
        if (timeout > (keyBits * 5)) {
          throw new Exception("Montgomery Multiplier Wait Timed out!")
        }
      }

      val actualOut = dut.io.out.bits.peek().litValue
      assert(actualOut == expected, f"Expected 0x$expected%x, got 0x$actualOut%x")

      dut.clock.step(1)
    }
  }
}