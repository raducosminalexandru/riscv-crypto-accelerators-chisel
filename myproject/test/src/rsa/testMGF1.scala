package rsa

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import sha._
import rsa.src.MGF1

class MGF1Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MGF1"

  it should "generate mask correctly for a given seed and length: example 1" in {
    test(new MGF1(32)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      
      dut.io.in.start.poke(true.B)
      dut.io.in.maskLen.poke(50.U)
      dut.io.out.hashOut.ready.poke(true.B)
      
      dut.clock.step(1)
      dut.io.in.start.poke(false.B)

      while (!dut.io.in.zData.ready.peek().litToBoolean) {
        dut.clock.step(1)
      }

      dut.io.in.zData.valid.poke(true.B)
      dut.io.in.zData.bits.data.poke(BigInt("61626300", 16).U)
      dut.io.in.zData.bits.validBits.poke(32.U)
      dut.io.in.zData.bits.isLast.poke(true.B)

      dut.clock.step(1)
      dut.io.in.zData.valid.poke(false.B)

      val expectedHashes = Array(
        BigInt("ce3172860f253e5bfdc1556198b48076f408dd224a83bbb75bd97a5f80374efd", 16),
        BigInt("af18396188c669c7e0ae83848ee6e706ed597a6049c1c1b16076cc6739ae31a3", 16)
      )

      var completedBlocks = 0
      var isLast = false
      var timeout = 0

      while (!isLast) {
        while (!dut.io.out.hashOut.valid.peek().litToBoolean) {
          dut.clock.step(1)
          timeout += 1
          if (timeout > 500) {
            throw new Exception("MGF1 Hash Wait Timed out!")
          }
        }

        val actualHash = dut.io.out.hashOut.bits.peek().litValue
        val validBytes = dut.io.out.validBytes.peek().litValue
        isLast = dut.io.out.isLastBlock.peek().litToBoolean

        assert(actualHash == expectedHashes(completedBlocks))

        if (isLast) {
            assert(validBytes == 18, s"Expected 18 valid bytes on last block, got $validBytes")
            } else {
            assert(validBytes == 32, s"Expected 32 valid bytes, got $validBytes")
        }

        completedBlocks += 1
        dut.clock.step(1)
        timeout = 0
      }

      assert(completedBlocks == 2)
      assert(isLast)
    }
  }

  it should "generate mask correctly for a given seed and length: example 2" in {
    test(new MGF1(32)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      
      dut.io.in.start.poke(true.B)
      dut.io.in.maskLen.poke(30.U)
      dut.io.out.hashOut.ready.poke(true.B)
      
      dut.clock.step(1)
      dut.io.in.start.poke(false.B)

      while (!dut.io.in.zData.ready.peek().litToBoolean) {
        dut.clock.step(1)
      }

      dut.io.in.zData.valid.poke(true.B)
      dut.io.in.zData.bits.data.poke(BigInt("92305912", 16).U)
      dut.io.in.zData.bits.validBits.poke(32.U)
      dut.io.in.zData.bits.isLast.poke(true.B)

      dut.clock.step(1)
      dut.io.in.zData.valid.poke(false.B)

      val expectedHash = Array(
        BigInt("37cf7b0810b97977c72481550bd1649e35aa48240805ef5763a2062921922b7a", 16),
      )

      var completedBlocks = 0
      var isLast = false
      var timeout = 0

      while (!isLast) {
        while (!dut.io.out.hashOut.valid.peek().litToBoolean) {
          dut.clock.step(1)
          timeout += 1
          if (timeout > 500) {
            throw new Exception("MGF1 Hash Wait Timed out!")
          }
        }

        val actualHash = dut.io.out.hashOut.bits.peek().litValue
        val validBytes = dut.io.out.validBytes.peek().litValue
        isLast = dut.io.out.isLastBlock.peek().litToBoolean

        assert(actualHash == expectedHash(completedBlocks))

        if (isLast) {
            assert(validBytes == 30, s"Expected 30 valid bytes on last block, got $validBytes")
        }

        completedBlocks += 1
        dut.clock.step(1)
        timeout = 0
      }

      assert(completedBlocks == 1)
      assert(isLast)
    }
  }
}