package rsa

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import rsa.src.OAEP_Decoding
import rsa.src.OAEP_Encoding

class OAEP_Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "OAEP Encoding and Decoding"

  val hLen     = 32
  val k        = 256
  val maxMLen  = k - 2 * hLen - 2

  it should "encode a message: example 1" in {
    test(new OAEP_Encoding(hLen, k)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      val goldenEM = Array(
        0.U, 124.U, 25.U, 121.U, 141.U, 174.U, 192.U, 14.U, 184.U, 214.U, 237.U, 244.U, 22.U, 27.U, 228.U,
        193.U, 140.U, 98.U, 68.U, 171.U, 36.U, 188.U, 4.U, 117.U, 184.U, 35.U, 115.U, 63.U, 223.U, 126.U,
        161.U, 236.U, 127.U, 147.U, 68.U, 196.U, 127.U, 202.U, 74.U, 247.U, 23.U, 64.U, 126.U, 218.U, 91.U,
        188.U, 4.U, 224.U, 162.U, 146.U, 122.U, 201.U, 212.U, 252.U, 32.U, 234.U, 63.U, 24.U, 198.U, 129.U,
        215.U, 30.U, 49.U, 194.U, 209.U, 4.U, 166.U, 149.U, 10.U, 6.U, 211.U, 227.U, 48.U, 138.U, 215.U,
        211.U, 96.U, 110.U, 248.U, 16.U, 235.U, 18.U, 78.U, 57.U, 67.U, 64.U, 76.U, 167.U, 70.U, 161.U,
        44.U, 81.U, 199.U, 191.U, 119.U, 104.U, 57.U, 15.U, 141.U, 132.U, 42.U, 201.U, 203.U, 98.U, 52.U,
        151.U, 121.U, 167.U, 83.U, 122.U, 120.U, 50.U, 125.U, 84.U, 90.U, 174.U, 179.U, 59.U, 45.U, 66.U,
        199.U, 209.U, 220.U, 54.U, 128.U, 164.U, 178.U, 54.U, 40.U, 98.U, 126.U, 157.U, 184.U, 173.U, 71.U,
        191.U, 231.U, 109.U, 190.U, 101.U, 61.U, 3.U, 210.U, 192.U, 163.U, 89.U, 153.U, 237.U, 40.U, 165.U,
        2.U, 57.U, 36.U, 21.U, 13.U, 114.U, 80.U, 134.U, 104.U, 210.U, 68.U, 47.U, 149.U, 219.U, 75.U, 10.U,
        125.U, 232.U, 128.U, 69.U, 139.U, 25.U, 150.U, 111.U, 33.U, 145.U, 143.U, 150.U, 68.U, 16.U, 110.U,
        141.U, 46.U, 180.U, 175.U, 242.U, 56.U, 69.U, 112.U, 60.U, 210.U, 20.U, 146.U, 12.U, 28.U, 155.U,
        11.U, 196.U, 53.U, 137.U, 2.U, 184.U, 35.U, 199.U, 103.U, 83.U, 32.U, 213.U, 157.U, 237.U, 35.U,
        79.U, 48.U, 139.U, 157.U, 250.U, 95.U, 141.U, 132.U, 77.U, 25.U, 120.U, 51.U, 12.U, 102.U, 159.U,
        168.U, 115.U, 7.U, 23.U, 104.U, 207.U, 70.U, 180.U, 25.U, 173.U, 40.U, 103.U, 187.U, 99.U, 18.U,
        183.U, 89.U, 0.U, 124.U, 174.U, 213.U, 5.U, 150.U, 108.U, 123.U, 245.U, 125.U, 112.U, 202.U, 33.U
      )

      dut.io.in.valid.poke(true.B)
      dut.io.out.ready.poke(true.B)

      val testMsg = "Chisel-RSA".getBytes
      for (i <- 0 until maxMLen) {
        if (i < testMsg.length) dut.io.in.bits.message(i).poke((testMsg(i) & 0xFF).U)
        else dut.io.in.bits.message(i).poke(0.U)
      }
      dut.io.in.bits.mLen.poke(testMsg.length.U)
      for (i <- 0 until hLen) {
        dut.io.in.bits.seed(i).poke(i.U)
      }

      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)

      var timeout = 0
      while (!dut.io.out.valid.peek().litToBoolean) {
        dut.clock.step(1)
        timeout += 1
        if (timeout > 50000) throw new Exception("Encoding timed out!")
      }

      dut.io.out.bits.encodedMessage(0).expect(0.U)
      for (i <- 0 until k) {
        dut.io.out.bits.encodedMessage(i).expect(goldenEM(i))
      }
    }
  }

  it should "encode a message: example 2" in {
    test(new OAEP_Encoding(hLen, k)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      val goldenEM = Array(
        0.U, 138.U, 52.U, 21.U, 4.U, 55.U, 44.U, 173.U, 128.U, 106.U, 110.U, 175.U, 81.U, 182.U, 69.U,
        125.U, 125.U, 166.U, 25.U, 196.U, 162.U, 255.U, 88.U, 43.U, 131.U, 164.U, 23.U, 34.U, 4.U, 109.U,
        13.U, 247.U, 27.U, 147.U, 68.U, 196.U, 127.U, 202.U, 74.U, 247.U, 23.U, 64.U, 126.U, 218.U, 91.U,
        188.U, 4.U, 224.U, 162.U, 146.U, 122.U, 201.U, 212.U, 252.U, 32.U, 234.U, 63.U, 24.U, 198.U, 129.U,
        215.U, 30.U, 49.U, 194.U, 209.U, 4.U, 166.U, 149.U, 10.U, 6.U, 211.U, 227.U, 48.U, 138.U, 215.U,
        211.U, 96.U, 110.U, 248.U, 16.U, 235.U, 18.U, 78.U, 57.U, 67.U, 64.U, 76.U, 167.U, 70.U, 161.U,
        44.U, 81.U, 199.U, 191.U, 119.U, 104.U, 57.U, 15.U, 141.U, 132.U, 42.U, 201.U, 203.U, 98.U, 52.U,
        151.U, 121.U, 167.U, 83.U, 122.U, 120.U, 50.U, 125.U, 84.U, 90.U, 174.U, 179.U, 59.U, 45.U, 66.U,
        199.U, 209.U, 220.U, 54.U, 128.U, 164.U, 178.U, 54.U, 40.U, 98.U, 126.U, 157.U, 184.U, 173.U, 71.U,
        191.U, 231.U, 109.U, 190.U, 101.U, 61.U, 3.U, 210.U, 192.U, 163.U, 89.U, 153.U, 237.U, 40.U, 165.U,
        2.U, 57.U, 36.U, 21.U, 13.U, 114.U, 80.U, 134.U, 104.U, 210.U, 68.U, 47.U, 149.U, 219.U, 75.U, 10.U,
        125.U, 232.U, 128.U, 69.U, 139.U, 25.U, 150.U, 111.U, 33.U, 145.U, 143.U, 150.U, 68.U, 16.U, 110.U,
        141.U, 46.U, 180.U, 175.U, 242.U, 56.U, 69.U, 112.U, 60.U, 210.U, 20.U, 146.U, 12.U, 28.U, 155.U,
        11.U, 196.U, 53.U, 137.U, 2.U, 184.U, 35.U, 199.U, 103.U, 83.U, 32.U, 213.U, 157.U, 237.U, 35.U,
        79.U, 48.U, 139.U, 157.U, 250.U, 95.U, 141.U, 132.U, 77.U, 25.U, 120.U, 51.U, 12.U, 102.U, 159.U,
        168.U, 115.U, 7.U, 23.U, 104.U, 207.U, 70.U, 180.U, 25.U, 173.U, 40.U, 103.U, 187.U, 99.U, 18.U,
        183.U, 88.U, 74.U, 25.U, 220.U, 227.U, 30.U, 223.U, 118.U, 109.U, 185.U, 28.U, 77.U, 235.U, 4.U
      )

      dut.io.in.valid.poke(true.B)
      dut.io.out.ready.poke(true.B)

      val testMsg = "Jesus is Lord".getBytes
      for (i <- 0 until maxMLen) {
        if (i < testMsg.length) dut.io.in.bits.message(i).poke((testMsg(i) & 0xFF).U)
        else dut.io.in.bits.message(i).poke(0.U)
      }
      dut.io.in.bits.mLen.poke(testMsg.length.U)
      for (i <- 0 until hLen) {
        dut.io.in.bits.seed(i).poke(i.U)
      }

      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)

      var timeout = 0
      while (!dut.io.out.valid.peek().litToBoolean) {
        dut.clock.step(1)
        timeout += 1
        if (timeout > 50000) throw new Exception("Encoding timed out!")
      }

      dut.io.out.bits.encodedMessage(0).expect(0.U)
      for (i <- 0 until k) {
        dut.io.out.bits.encodedMessage(i).expect(goldenEM(i))
      }
    }
  }

  it should "decode a message: decode example 1" in {
    test(new OAEP_Decoding(hLen, k)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      val inputEM = Array(
        0.U, 124.U, 25.U, 121.U, 141.U, 174.U, 192.U, 14.U, 184.U, 214.U, 237.U, 244.U, 22.U, 27.U, 228.U,
        193.U, 140.U, 98.U, 68.U, 171.U, 36.U, 188.U, 4.U, 117.U, 184.U, 35.U, 115.U, 63.U, 223.U, 126.U,
        161.U, 236.U, 127.U, 147.U, 68.U, 196.U, 127.U, 202.U, 74.U, 247.U, 23.U, 64.U, 126.U, 218.U, 91.U,
        188.U, 4.U, 224.U, 162.U, 146.U, 122.U, 201.U, 212.U, 252.U, 32.U, 234.U, 63.U, 24.U, 198.U, 129.U,
        215.U, 30.U, 49.U, 194.U, 209.U, 4.U, 166.U, 149.U, 10.U, 6.U, 211.U, 227.U, 48.U, 138.U, 215.U,
        211.U, 96.U, 110.U, 248.U, 16.U, 235.U, 18.U, 78.U, 57.U, 67.U, 64.U, 76.U, 167.U, 70.U, 161.U,
        44.U, 81.U, 199.U, 191.U, 119.U, 104.U, 57.U, 15.U, 141.U, 132.U, 42.U, 201.U, 203.U, 98.U, 52.U,
        151.U, 121.U, 167.U, 83.U, 122.U, 120.U, 50.U, 125.U, 84.U, 90.U, 174.U, 179.U, 59.U, 45.U, 66.U,
        199.U, 209.U, 220.U, 54.U, 128.U, 164.U, 178.U, 54.U, 40.U, 98.U, 126.U, 157.U, 184.U, 173.U, 71.U,
        191.U, 231.U, 109.U, 190.U, 101.U, 61.U, 3.U, 210.U, 192.U, 163.U, 89.U, 153.U, 237.U, 40.U, 165.U,
        2.U, 57.U, 36.U, 21.U, 13.U, 114.U, 80.U, 134.U, 104.U, 210.U, 68.U, 47.U, 149.U, 219.U, 75.U, 10.U,
        125.U, 232.U, 128.U, 69.U, 139.U, 25.U, 150.U, 111.U, 33.U, 145.U, 143.U, 150.U, 68.U, 16.U, 110.U,
        141.U, 46.U, 180.U, 175.U, 242.U, 56.U, 69.U, 112.U, 60.U, 210.U, 20.U, 146.U, 12.U, 28.U, 155.U,
        11.U, 196.U, 53.U, 137.U, 2.U, 184.U, 35.U, 199.U, 103.U, 83.U, 32.U, 213.U, 157.U, 237.U, 35.U,
        79.U, 48.U, 139.U, 157.U, 250.U, 95.U, 141.U, 132.U, 77.U, 25.U, 120.U, 51.U, 12.U, 102.U, 159.U,
        168.U, 115.U, 7.U, 23.U, 104.U, 207.U, 70.U, 180.U, 25.U, 173.U, 40.U, 103.U, 187.U, 99.U, 18.U,
        183.U, 89.U, 0.U, 124.U, 174.U, 213.U, 5.U, 150.U, 108.U, 123.U, 245.U, 125.U, 112.U, 202.U, 33.U
      )

      val expectedMsg = "Chisel-RSA".getBytes

      dut.io.in.valid.poke(true.B)
      dut.io.out.ready.poke(true.B)

      for (i <- 0 until k) {
        dut.io.in.bits.encodedMessage(i).poke(inputEM(i))
      }

      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)

      var timeout = 0
      while (!dut.io.out.valid.peek().litToBoolean) {
        dut.clock.step(1)
        timeout += 1
        if (timeout > 50000) throw new Exception("Decoding timed out!")
      }

      dut.io.out.bits.isError.expect(false.B)
      dut.io.out.bits.mLen.expect(expectedMsg.length.U)

      for (i <- 0 until expectedMsg.length) {
        dut.io.out.bits.message(i).expect((expectedMsg(i) & 0xFF).U)
      }
    }
  }

  it should "decode a message: decode example 2" in {
    test(new OAEP_Decoding(hLen, k)) { dut =>
      dut.clock.setTimeout(0)

      val inputEM = Array(
        0.U, 138.U, 52.U, 21.U, 4.U, 55.U, 44.U, 173.U, 128.U, 106.U, 110.U, 175.U, 81.U, 182.U, 69.U,
        125.U, 125.U, 166.U, 25.U, 196.U, 162.U, 255.U, 88.U, 43.U, 131.U, 164.U, 23.U, 34.U, 4.U, 109.U,
        13.U, 247.U, 27.U, 147.U, 68.U, 196.U, 127.U, 202.U, 74.U, 247.U, 23.U, 64.U, 126.U, 218.U, 91.U,
        188.U, 4.U, 224.U, 162.U, 146.U, 122.U, 201.U, 212.U, 252.U, 32.U, 234.U, 63.U, 24.U, 198.U, 129.U,
        215.U, 30.U, 49.U, 194.U, 209.U, 4.U, 166.U, 149.U, 10.U, 6.U, 211.U, 227.U, 48.U, 138.U, 215.U,
        211.U, 96.U, 110.U, 248.U, 16.U, 235.U, 18.U, 78.U, 57.U, 67.U, 64.U, 76.U, 167.U, 70.U, 161.U,
        44.U, 81.U, 199.U, 191.U, 119.U, 104.U, 57.U, 15.U, 141.U, 132.U, 42.U, 201.U, 203.U, 98.U, 52.U,
        151.U, 121.U, 167.U, 83.U, 122.U, 120.U, 50.U, 125.U, 84.U, 90.U, 174.U, 179.U, 59.U, 45.U, 66.U,
        199.U, 209.U, 220.U, 54.U, 128.U, 164.U, 178.U, 54.U, 40.U, 98.U, 126.U, 157.U, 184.U, 173.U, 71.U,
        191.U, 231.U, 109.U, 190.U, 101.U, 61.U, 3.U, 210.U, 192.U, 163.U, 89.U, 153.U, 237.U, 40.U, 165.U,
        2.U, 57.U, 36.U, 21.U, 13.U, 114.U, 80.U, 134.U, 104.U, 210.U, 68.U, 47.U, 149.U, 219.U, 75.U, 10.U,
        125.U, 232.U, 128.U, 69.U, 139.U, 25.U, 150.U, 111.U, 33.U, 145.U, 143.U, 150.U, 68.U, 16.U, 110.U,
        141.U, 46.U, 180.U, 175.U, 242.U, 56.U, 69.U, 112.U, 60.U, 210.U, 20.U, 146.U, 12.U, 28.U, 155.U,
        11.U, 196.U, 53.U, 137.U, 2.U, 184.U, 35.U, 199.U, 103.U, 83.U, 32.U, 213.U, 157.U, 237.U, 35.U,
        79.U, 48.U, 139.U, 157.U, 250.U, 95.U, 141.U, 132.U, 77.U, 25.U, 120.U, 51.U, 12.U, 102.U, 159.U,
        168.U, 115.U, 7.U, 23.U, 104.U, 207.U, 70.U, 180.U, 25.U, 173.U, 40.U, 103.U, 187.U, 99.U, 18.U,
        183.U, 88.U, 74.U, 25.U, 220.U, 227.U, 30.U, 223.U, 118.U, 109.U, 185.U, 28.U, 77.U, 235.U, 4.U
      )

      val expectedMsg = "Jesus is Lord".getBytes

      dut.io.in.valid.poke(true.B)
      dut.io.out.ready.poke(true.B)

      for (i <- 0 until k) {
        dut.io.in.bits.encodedMessage(i).poke(inputEM(i))
      }

      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)

      var timeout = 0
      while (!dut.io.out.valid.peek().litToBoolean) {
        dut.clock.step(1)
        timeout += 1
        if (timeout > 50000) throw new Exception("Decoding timed out!")
      }

      dut.io.out.bits.isError.expect(false.B)
      dut.io.out.bits.mLen.expect(expectedMsg.length.U)

      for (i <- 0 until expectedMsg.length) {
        dut.io.out.bits.message(i).expect((expectedMsg(i) & 0xFF).U)
      }
    }
  }
}