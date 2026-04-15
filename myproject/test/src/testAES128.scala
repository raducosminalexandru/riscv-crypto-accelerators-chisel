package aes

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AES128Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AES128"

  def runAES(dut: AES128, input: BigInt, key: BigInt): BigInt = {
    dut.io.key.poke(key.U)
    
    dut.io.plaintext.poke(input.U)
    dut.io.isDecrypt.poke(false.B)
    dut.io.start.poke(true.B)
    dut.clock.step(1)
    dut.io.start.poke(false.B)

    while (!dut.io.done.peek().litToBoolean) {
      dut.clock.step(1)
    }
    val encrypted = dut.io.out.peek().litValue
    println(s"Input:     ${input.toString(16).reverse.padTo(32, '0').reverse}")
    println(s"Encrypted: ${encrypted.toString(16).reverse.padTo(32, '0').reverse}")

    dut.io.plaintext.poke(encrypted.U)
    dut.io.isDecrypt.poke(true.B)
    dut.io.start.poke(true.B)
    dut.clock.step(1)
    dut.io.start.poke(false.B)

    var timeout = 0
    
    while (!dut.io.done.peek().litToBoolean) {
        dut.clock.step(1)
        timeout += 1
        if (timeout > 50) {
            throw new Exception("AES Timed out!")
        }
    }
    
    val decrypted = dut.io.out.peek().litValue
    println(s"Decrypted: ${decrypted.toString(16).reverse.padTo(32, '0').reverse}")
    println("-" * 40)
    
    decrypted
  }

  it should "perform encryption and decryption with the FIPS-197 key" in {
    test(new AES128()) { dut =>
      dut.clock.setTimeout(0)
      val plaintext = BigInt("3243f6a8885a308d313198a2e0370734", 16)
      val key = BigInt("2b7e151628aed2a6abf7158809cf4f3c", 16)

      val result = runAES(dut, plaintext, key)
      assert(result == plaintext, "Standard vector decryption failed")
    }
  }

  it should "handle all-zero plaintext and random key" in {
    test(new AES128()) { dut =>
      dut.clock.setTimeout(0)
      val zeroVal = BigInt(0)
      val rand = new scala.util.Random
      val randKey = BigInt(128, rand)
      
      val result = runAES(dut, zeroVal, randKey)
      assert(result == zeroVal, "Zero-block symmetry check failed")
    }
  }

  it should "handle all-ones plaintext and random key" in {
    test(new AES128()) { dut =>
      dut.clock.setTimeout(0)
      val onesVal = (BigInt(1) << 128) - 1
      val rand = new scala.util.Random
      val randKey = BigInt(128, rand)
      
      val result = runAES(dut, onesVal, randKey)
      assert(result == onesVal, "Ones-block symmetry check failed")
    }
  }

  it should "work with random keys and data across multiple iterations" in {
    test(new AES128()) { dut =>
      dut.clock.setTimeout(0)
      val rand = new scala.util.Random
      
      for (i <- 1 to 5) {
        val randPlaintext = BigInt(128, rand)
        val randKey = BigInt(128, rand)
        
        val result = runAES(dut, randPlaintext, randKey)
        assert(result == randPlaintext, s"Random test iteration $i failed")
      }
    }
  }
}

class AES128EncryptionTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AES128 Encryption"

  it should "match the FIPS-197 standard encryption vector" in {
    test(new AES128()) { dut =>
      val plaintext = BigInt("3243f6a8885a308d313198a2e0370734", 16)
      val expectedCiphertext = BigInt("3925841d02dc09fbdc118597196a0b32", 16)
      
      // We now only need the root key; the module handles the expansion
      val key = BigInt("2b7e151628aed2a6abf7158809cf4f3c", 16)

      dut.io.key.poke(key.U)
      dut.io.plaintext.poke(plaintext.U)
      dut.io.isDecrypt.poke(false.B)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      var cycles = 0
      while (!dut.io.done.peek().litToBoolean && cycles < 20) {
        dut.clock.step(1)
        cycles += 1
      }

      val actualCiphertext = dut.io.out.peek().litValue
      
      println(s"Plaintext:  ${plaintext.toString(16).reverse.padTo(32, '0').reverse}")
      println(s"Expected:   ${expectedCiphertext.toString(16).reverse.padTo(32, '0').reverse}")
      println(s"Actual:     ${actualCiphertext.toString(16).reverse.padTo(32, '0').reverse}")

      assert(actualCiphertext == expectedCiphertext, "Encryption result does not match FIPS-197 vector!")
      assert(dut.io.done.peek().litToBoolean, "Module failed to assert 'done' signal")
    }
  }
}