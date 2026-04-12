package sha

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SHA256Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SHA256"

  def runSHA256(dut: SHA256, text: String): BigInt = {
    val bytes = text.getBytes("UTF-8")
    var i = 0

    dut.io.in.valid.poke(false.B)
    dut.io.out.ready.poke(true.B)

    if (bytes.isEmpty) {
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.data.poke(0.U)
      dut.io.in.bits.validBits.poke(0.U)
      dut.io.in.bits.isLast.poke(true.B)

      while (!dut.io.in.ready.peek().litToBoolean) {
        dut.clock.step(1)
      }
      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)
    } else {
      while (i < bytes.length) {
        val remaining = bytes.length - i
        val take = math.min(remaining, 4)
        
        var chunk = BigInt(0)
        for (b <- 0 until take) {
          chunk = (chunk << 8) | (bytes(i + b) & 0xFF)
        }
        
        val isLast = (i + take) == bytes.length

        dut.io.in.valid.poke(true.B)
        dut.io.in.bits.data.poke(chunk.U)
        dut.io.in.bits.validBits.poke((take * 8).U)
        dut.io.in.bits.isLast.poke(isLast.B)

        while (!dut.io.in.ready.peek().litToBoolean) {
          dut.clock.step(1)
        }
        dut.clock.step(1)
        i += take
      }
      dut.io.in.valid.poke(false.B)
    }

    var timeout = 0
    while (!dut.io.out.valid.peek().litToBoolean) {
      dut.clock.step(1)
      timeout += 1
      if (timeout > 500) {
        throw new Exception("SHA256 test timed out waiting for out.valid!")
      }
    }

    val hashResult = dut.io.out.bits.peek().litValue
    dut.clock.step(1)

    val displayString = if (text.length > 100) text.substring(0, 50) + "... [Truncated, Total Length: " + text.length + "]" else text
    println(s"Input String: '$displayString'")
    println(s"Hash Output:  ${hashResult.toString(16).reverse.padTo(64, '0').reverse}")
    println("-" * 64)

    hashResult
  }

  it should "correctly hash an empty string" in {
    test(new SHA256()) { dut =>
      dut.clock.setTimeout(0)
      
      val expected = BigInt("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", 16)
      val actual = runSHA256(dut, "")
      
      assert(actual == expected, "Empty string hash did not match expected FIPS vector.")
    }
  }

  it should "correctly hash the NIST short test vector 'abc'" in {
    test(new SHA256()) { dut =>
      dut.clock.setTimeout(0)
      
      val expected = BigInt("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad", 16)
      val actual = runSHA256(dut, "abc")
      
      assert(actual == expected, "'abc' string hash did not match expected FIPS vector.")
    }
  }

  it should "correctly hash a 56-byte multi-block edge case string" in {
    test(new SHA256()) { dut =>
      dut.clock.setTimeout(0)
      
      val text = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
      val expected = BigInt("248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1", 16)
      val actual = runSHA256(dut, text)
      
      assert(actual == expected, "56-byte multi-block string hash did not match expected FIPS vector.")
    }
  }

  it should "correctly hash a 112-byte multi-block string" in {
    test(new SHA256()) { dut =>
      dut.clock.setTimeout(0)
      
      val text = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
      val expected = BigInt("cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1", 16)
      val actual = runSHA256(dut, text)
      
      assert(actual == expected, "112-byte multi-block string hash did not match expected FIPS vector.")
    }
  }

  it should "correctly hash a message with non-ASCII characters" in {
    test(new SHA256()) { dut =>
      dut.clock.setTimeout(0)
      val text = "SHA is 🔥"
      val expected = BigInt("957a457a9c081184b6bfd81f878703c3b223ff5b7ace58a14b0c487da5f4bf6e", 16)
      val actual = runSHA256(dut, text)
      assert(actual == expected)
    }
  }

  it should "correctly hash a long repeating sequence" in {
    test(new SHA256()) { dut =>
      dut.clock.setTimeout(0)
      val text = "0123456789" * 10
      val expected = BigInt("9cfe7faff7054298ca87557e15a10262de8d3eee77827417fbdfea1c41b9ec23", 16)
      val actual = runSHA256(dut, text)
      assert(actual == expected)
    }
  }
}