package rsa

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import rsa.src.ModExp

class ModExpTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ModExp"

  it should "compute modular exponentiation correctly: example 1" in {
    test(new ModExp(32)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step(1)
      }

      // Mathematical setup:
      // We want to calculate: 5^3 mod 13
      // For Montgomery, R = 2^keyBits = 2^32
      // R % 13 = (2^32) % 13 = 9
      // R^2 % 13 = (9 * 9) % 13 = 81 % 13 = 3
      
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.base.poke(5.U)
      dut.io.in.bits.exp.poke(3.U)
      dut.io.in.bits.n.poke(13.U)
      dut.io.in.bits.r2.poke(3.U) 

      dut.io.out.ready.poke(true.B)
      
      dut.clock.step(1)
      dut.io.in.valid.poke(false.B) 

      var timeout = 0

      while (!dut.io.out.valid.peek().litToBoolean) {
        dut.clock.step(1)
        timeout += 1
        if (timeout > 10000) {
          throw new Exception("ModExp Computation Wait Timed out!")
        }
      }

      val actualResult = dut.io.out.bits.peek().litValue
      
      assert(actualResult == 8, s"Expected 8, got $actualResult")

      dut.clock.step(1)
    }
  }

  it should "compute modular exponentiation correctly: example 2" in {
    test(new ModExp(32)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      
      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step(1)
      }

      // Mathematical setup:
      // We want to calculate: 7^4 mod 251
      // R = 2^32
      // R % 251 = (2^32) % 251 = 123
      // R^2 % 251 = (123 * 123) % 251 = 15129 % 251 = 69
      
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.base.poke(7.U)
      dut.io.in.bits.exp.poke(4.U)
      dut.io.in.bits.n.poke(251.U)
      dut.io.in.bits.r2.poke(69.U)
      dut.io.out.ready.poke(true.B)

      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)

      var timeout = 0

      while (!dut.io.out.valid.peek().litToBoolean) {
        dut.clock.step(1)
        timeout += 1
        if (timeout > 10000) {
          throw new Exception("ModExp Computation Wait Timed out!")
        }
      }

      val actualResult = dut.io.out.bits.peek().litValue
      
      assert(actualResult == 142, s"Expected 142, got $actualResult")

      dut.clock.step(1)
    }
  }
}