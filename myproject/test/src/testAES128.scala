package aes

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AES128Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AES128"

  def runAES(dut: AES128, input: BigInt, keys: Seq[BigInt]): BigInt = {
    for (i <- 0 until 11) {
      dut.io.roundKeys(i).poke(keys(i).U)
    }
    
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
            throw new Exception(s"AES Timed out! Current Round: ${dut.roundCounter.peek().litValue}")
        }
    }

    dut.io.out.peek().litValue
    
    val decrypted = dut.io.out.peek().litValue
    println(s"Decrypted: ${decrypted.toString(16).reverse.padTo(32, '0').reverse}")
    println("-" * 40)
    
    decrypted
  }

  it should "perform encryption and decryption with provided round keys" in {
    test(new AES128()) { dut =>
      dut.clock.setTimeout(0)
      val plaintext = BigInt("3243f6a8885a308d313198a2e0370734", 16)
      val roundKeys = Seq(
        BigInt("2b7e151628aed2a6abf7158809cf4f3c", 16),
        BigInt("a0fafe1788542cb123a35fa35d0a240b", 16),
        BigInt("d4d035e172275ef21fa21997f66b579a", 16),
        BigInt("e9f505889bd25b7a847042edf21b1560", 16),
        BigInt("4e5f844e130282369772c0d96569d5bf", 16),
        BigInt("219139383293bb0ea5e17bd7c088aeb8", 16),
        BigInt("42296ce370ba57ed555b2c3a95d38282", 16),
        BigInt("d8d473a2a86e244ff035087565e68af7", 16),
        BigInt("23304b128b5e6f5d7b6b67281e8dee9f", 16),
        BigInt("4871e860c32f873db844e015a6c90e8a", 16),
        BigInt("ac7766f319fadc2128d12941575c006e", 16)
      )

      val result = runAES(dut, plaintext, roundKeys)
      assert(result == plaintext, "Standard vector decryption failed")
    }
  }

  it should "handle all-zero plaintext and random keys" in {
    test(new AES128()) { dut =>
      dut.clock.setTimeout(0)
      val zeroVal = BigInt(0)
      val rand = new scala.util.Random
      val zeroKeys = Seq.fill(11)(BigInt(128, rand))
      
      val result = runAES(dut, zeroVal, zeroKeys)
      assert(result == zeroVal, "Zero-block symmetry check failed")
    }
  }

  it should "handle all-ones plaintext and random round keys" in {
    test(new AES128()) { dut =>
      dut.clock.setTimeout(0)
      val onesVal = (BigInt(1) << 128) - 1
      val rand = new scala.util.Random
      val randKeys = Seq.fill(11)(BigInt(128, rand))
      
      val result = runAES(dut, onesVal, randKeys)
      assert(result == onesVal, "Ones-block symmetry check failed")
    }
  }


  it should "work with random keys and data across multiple iterations" in {
    test(new AES128()) { dut =>
      dut.clock.setTimeout(0)
      val rand = new scala.util.Random
      
      for (i <- 1 to 5) {
        val randPlaintext = BigInt(128, rand)
        val randKeys = Seq.fill(11)(BigInt(128, rand))
        
        val result = runAES(dut, randPlaintext, randKeys)
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
      

      val roundKeys = Seq(
        BigInt("2b7e151628aed2a6abf7158809cf4f3c", 16), 
        BigInt("a0fafe1788542cb123a339392a6c7605", 16), 
        BigInt("f2c295f27a96b9435935807a7359f67f", 16), 
        BigInt("3d80477d4716fe3e1e237e446d7a883b", 16), 
        BigInt("ef44a541a8525b7fb671253bdb0bad00", 16), 
        BigInt("d4d1c6f87c839d87caf2b8bc11f915bc", 16), 
        BigInt("6d88a37a110b3efddbf98641ca0093fd", 16),
        BigInt("4e54f70e5f5fc9f384a64fb24ea6dc4f", 16), 
        BigInt("ead27321b58dbad2312bf5607f8d292f", 16), 
        BigInt("ac7766f319fadc2128d12941575c006e", 16), 
        BigInt("d014f9a8c9ee2589e13f0cc8b6630ca6", 16)  
      )

      for (i <- 0 until 11) {
        dut.io.roundKeys(i).poke(roundKeys(i).U)
      }
      
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