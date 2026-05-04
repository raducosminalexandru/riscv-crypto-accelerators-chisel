package rsa

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import rsa.src.RSAES_OAEP
import scala.util.Random

class RSAES_OAEPTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "RSAES_OAEP"

  it should "encrypt and decrypt correctly: example 1" in {
    val testKeyBits = 576 
    
    test(new RSAES_OAEP(testKeyBits)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(15000000)

      val nVal   = BigInt("589d386b823f779e38188dc0181e01e54168f8eec8c0af2050cabd5856506fe9c0f1d8dcc86143017fc267bf5e1b0b0c3ee3185f2b6a55b81aeb4f96d33073c4743b05998f2d898d", 16)
      val eVal   = BigInt("10001", 16)
      val dVal   = BigInt("1faf2f8239530a29377388139c4bf66f8b80c62952fc9a619aa1b32c07c8c10b3aa2fee01ecc2a44e265b4a4e3ca463b74757cfdcfa079664e27e1b26b8baf306a55486adf07055d", 16)
      val r2Val  = BigInt("502bf9e669f07ab2971f58871332e91d741f08774f438e115ae9640cca94a131856f7509e3f6537142fa1953bc1aabb817893e0b679f658963bcdbad2782c5683ce701a7394a46db", 16)
      val msgVal = BigInt("cafebabe", 16)
      val msgLen = 4

      dut.io.in.bits.isDecrypt.poke(false.B)
      dut.io.in.bits.n.poke(nVal.U)
      dut.io.in.bits.exp.poke(eVal.U)
      dut.io.in.bits.dataIn.poke(msgVal.U)
      dut.io.in.bits.dataLen.poke(msgLen.U)
      dut.io.in.bits.r2.poke(r2Val.U)
      for (i <- 0 until 32) {
        dut.io.in.bits.seed(i).poke(Random.nextInt(256).U) // simulate random seed for encryption
      }

      dut.io.in.valid.poke(true.B)

      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step(1)
      }
      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)

      dut.io.out.ready.poke(true.B)
      while (!dut.io.out.valid.peek().litToBoolean) {
        dut.clock.step(1)
      }

      val cipherText = dut.io.out.bits.c.peek().litValue
      dut.clock.step(1)
      dut.io.out.ready.poke(false.B)

      dut.io.in.bits.isDecrypt.poke(true.B)
      dut.io.in.bits.n.poke(nVal.U)
      dut.io.in.bits.exp.poke(dVal.U)
      dut.io.in.bits.dataIn.poke(cipherText.U)
      dut.io.in.bits.dataLen.poke(0.U)
      dut.io.in.bits.r2.poke(r2Val.U)
      for (i <- 0 until 32) dut.io.in.bits.seed(i).poke(0.U)

      dut.io.in.valid.poke(true.B)

      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step(1)
      }
      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)

      dut.io.out.ready.poke(true.B) 
      while (!dut.io.out.valid.peek().litToBoolean) {
        dut.clock.step(1)
      }

      val decodedMsg = dut.io.out.bits.c.peek().litValue
      val decodedLen = dut.io.out.bits.mLen.peek().litValue
      val isError = dut.io.out.bits.isError.peek().litToBoolean
      dut.clock.step(1)

      assert(decodedMsg == msgVal, s"Expected Msg ${msgVal.toString(16)}, got ${decodedMsg.toString(16)}")
      assert(decodedLen == msgLen, s"Expected Len $msgLen, got $decodedLen")
      assert(!isError, "OAEP Decoder threw an error flag!")
    }
  }

  it should "encrypt and decrypt correctly: example 2" in {
    val testKeyBits = 576 
    
    test(new RSAES_OAEP(testKeyBits)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(15000000)

      val keyBits = 576
      val nVal   = BigInt("5888948c357a89f1973e1967fd549f76dee188f6a0f3532a26bba5348af9229334fca9e62e8680386b994e54fc62b68731e3f8e36cddf545be9a6d747c7fe79e65742b14797f5e19", 16)
      val eVal   = BigInt("10001", 16)
      val dVal   = BigInt("1c1d636f131c92e9d1a9bacfb737445caf72d7d7da4ceb401b84c375e81c2dcaa9ad0316e20ed1b2b90e8e137131be5b7255072f8653c1cbded5e27fde2294a0855a42763cc20201", 16)
      val r2Val  = BigInt("41c4ec0f331259723d611cdf0b6416185f2ebba6db449173d2455d857dd4ec9d2470edee2029d531283af3307893dc6a161a2b50ed7642888744cb804c9c7496cee719f90fdf7513", 16)
      val msgVal = BigInt("deadbeef", 16)
      val msgLen = 4

      dut.io.in.bits.isDecrypt.poke(false.B)
      dut.io.in.bits.n.poke(nVal.U)
      dut.io.in.bits.exp.poke(eVal.U)
      dut.io.in.bits.dataIn.poke(msgVal.U)
      dut.io.in.bits.dataLen.poke(msgLen.U)
      dut.io.in.bits.r2.poke(r2Val.U)
      for (i <- 0 until 32) {
        dut.io.in.bits.seed(i).poke(Random.nextInt(256).U) // simulate random seed for encryption
      }

      dut.io.in.valid.poke(true.B)

      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step(1)
      }
      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)

      dut.io.out.ready.poke(true.B)
      while (!dut.io.out.valid.peek().litToBoolean) {
        dut.clock.step(1)
      }

      val cipherText = dut.io.out.bits.c.peek().litValue
      dut.clock.step(1)
      dut.io.out.ready.poke(false.B)

      dut.io.in.bits.isDecrypt.poke(true.B)
      dut.io.in.bits.n.poke(nVal.U)
      dut.io.in.bits.exp.poke(dVal.U)
      dut.io.in.bits.dataIn.poke(cipherText.U)
      dut.io.in.bits.dataLen.poke(0.U)
      dut.io.in.bits.r2.poke(r2Val.U)
      for (i <- 0 until 32) dut.io.in.bits.seed(i).poke(0.U)

      dut.io.in.valid.poke(true.B)

      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step(1)
      }
      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)

      dut.io.out.ready.poke(true.B) 
      while (!dut.io.out.valid.peek().litToBoolean) {
        dut.clock.step(1)
      }

      val decodedMsg = dut.io.out.bits.c.peek().litValue
      val decodedLen = dut.io.out.bits.mLen.peek().litValue
      val isError = dut.io.out.bits.isError.peek().litToBoolean
      dut.clock.step(1)

      assert(decodedMsg == msgVal, s"Expected Msg ${msgVal.toString(16)}, got ${decodedMsg.toString(16)}")
      assert(decodedLen == msgLen, s"Expected Len $msgLen, got $decodedLen")
      assert(!isError, "OAEP Decoder threw an error flag!")
    }
  }
}