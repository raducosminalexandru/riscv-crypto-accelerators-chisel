package sha.src

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR

class SHA256Input extends Bundle {
  val data = UInt(32.W)
  val validBits = UInt(6.W)
  val isLast = Bool()
}

class SHA256Interface extends Bundle {
  val in = Flipped(Decoupled(new SHA256Input()))
  val out = Decoupled(UInt(512.W))
  val lastBlock = Output(Bool())
}

class SHA256 extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SHA256Input()))
    //val debug_injectFault = Input(Bool()) - For testing AFA attacks only --- IGNORE ---
    val out = Decoupled(UInt(256.W))
  })

  val padder = Module(new SHA256Padder())
  padder.io.in <> io.in

  val K = VecInit(Seq(
    "h428a2f98", "h71374491", "hb5c0fbcf", "he9b5dba5", "h3956c25b", "h59f111f1", "h923f82a4", "hab1c5ed5",
    "hd807aa98", "h12835b01", "h243185be", "h550c7dc3", "h72be5d74", "h80deb1fe", "h9bdc06a7", "hc19bf174",
    "he49b69c1", "hefbe4786", "h0fc19dc6", "h240ca1cc", "h2de92c6f", "h4a7484aa", "h5cb0a9dc", "h76f988da",
    "h983e5152", "ha831c66d", "hb00327c8", "hbf597fc7", "hc6e00bf3", "hd5a79147", "h06ca6351", "h14292967",
    "h27b70a85", "h2e1b2138", "h4d2c6dfc", "h53380d13", "h650a7354", "h766a0abb", "h81c2c92e", "h92722c85",
    "ha2bfe8a1", "ha81a664b", "hc24b8b70", "hc76c51a3", "hd192e819", "hd6990624", "hf40e3585", "h106aa070",
    "h19a4c116", "h1e376c08", "h2748774c", "h34b0bcb5", "h391c0cb3", "h4ed8aa4a", "h5b9cca4f", "h682e6ff3",
    "h748f82ee", "h78a5636f", "h84c87814", "h8cc70208", "h90befffa", "ha4506ceb", "hbef9a3f7", "hc67178f2"
  ).map(hexString => hexString.U(32.W)))

  val H_INIT = Seq(
    "h6a09e667".U(32.W), "hbb67ae85".U(32.W), "h3c6ef372".U(32.W), "ha54ff53a".U(32.W),
    "h510e527f".U(32.W), "h9b05688c".U(32.W), "h1f83d9ab".U(32.W), "h5be0cd19".U(32.W)
  )
  val H = RegInit(VecInit(H_INIT))
  
  val dummy_W = Reg(Vec(16, UInt(32.W)))
  val dummy_a = RegInit("h01234567".U(32.W))
  val dummy_b = RegInit("h89abcdef".U(32.W))
  val dummy_c = RegInit("hdeadbeef".U(32.W))
  val dummy_d = RegInit("hcafebabe".U(32.W))
  val dummy_e = RegInit("h1337c0de".U(32.W))
  val dummy_f = RegInit("h8badf00d".U(32.W))
  val dummy_g = RegInit("hfeedface".U(32.W))
  val dummy_h = RegInit("hbaadf00d".U(32.W))
  val dummy_round = RegInit(0.U(7.W))

  val W = Reg(Vec(16, UInt(32.W)))

  val a = Reg(UInt(32.W))
  val b = Reg(UInt(32.W))
  val c = Reg(UInt(32.W))
  val d = Reg(UInt(32.W))
  val e = Reg(UInt(32.W))
  val f = Reg(UInt(32.W))
  val g = Reg(UInt(32.W))
  val h = Reg(UInt(32.W))

  val round = RegInit(0.U(7.W))
  val isLastBlock = RegInit(false.B)

  val s_idle :: s_load :: s_compress :: s_update :: s_done :: Nil = Enum(5)
  val state = RegInit(s_idle)

  def ROTR(x: UInt, n: Int): UInt = (x >> n) | (x << (32 - n))(31, 0)
  def SHR(x: UInt, n: Int): UInt  = x >> n

  def Ch(x: UInt, y: UInt, z: UInt)  = (x & y) ^ (~x & z)
  def Maj(x: UInt, y: UInt, z: UInt) = (x & y) ^ (x & z) ^ (y & z)
  def Sigma0(x: UInt) = ROTR(x, 2) ^ ROTR(x, 13) ^ ROTR(x, 22)
  def Sigma1(x: UInt) = ROTR(x, 6) ^ ROTR(x, 11) ^ ROTR(x, 25)
  def sigma0(x: UInt) = ROTR(x, 7) ^ ROTR(x, 18) ^ SHR(x, 3)
  def sigma1(x: UInt) = ROTR(x, 17) ^ ROTR(x, 19) ^ SHR(x, 10)

  padder.io.out.ready := (state === s_idle)
  io.out.valid := (state === s_done)
  
  io.out.bits := Cat(H(0), H(1), H(2), H(3), H(4), H(5), H(6), H(7))

  val randomBits = LFSR(16)
  val stall = randomBits(0)

  switch(state) {
    is(s_idle) {
      when(padder.io.out.valid) {
        for(i <- 0 until 16) {
          W(i) := padder.io.out.bits(511 - i * 32, 480 - i * 32)
        }
        isLastBlock := padder.io.lastBlock
        state := s_load
      }
    }

    is(s_load) {
      a := H(0); b := H(1); c := H(2); d := H(3)
      e := H(4); f := H(5); g := H(6); h := H(7)
      round := 0.U
      state := s_compress
    }

    is(s_compress) {
      val active_a = Mux(stall, dummy_a, a)
      val active_b = Mux(stall, dummy_b, b)
      val active_c = Mux(stall, dummy_c, c)
      val active_d = Mux(stall, dummy_d, d)
      val active_e = Mux(stall, dummy_e, e)
      val active_f = Mux(stall, dummy_f, f)
      val active_g = Mux(stall, dummy_g, g)
      val active_h = Mux(stall, dummy_h, h)
      val active_round = Mux(stall, dummy_round, round)
      
      val active_W_0  = Mux(stall, dummy_W(0), W(0))
      val active_W_1  = Mux(stall, dummy_W(1), W(1))
      val active_W_9  = Mux(stall, dummy_W(9), W(9))
      val active_W_14 = Mux(stall, dummy_W(14), W(14))

      val w_t = Wire(UInt(32.W))
      
      when(active_round < 16.U) {
        w_t := active_W_0
      }.otherwise {
        w_t := active_W_0 + sigma0(active_W_1) + active_W_9 + sigma1(active_W_14)
      }

      val temp1 = active_h + Sigma1(active_e) + Ch(active_e, active_f, active_g) + K(active_round(5,0)) + w_t
      val temp2 = Sigma0(active_a) + Maj(active_a, active_b, active_c)

      val next_a = temp1 + temp2
      val next_e = active_d + temp1
      val next_round = active_round + 1.U

      when(!stall) {
        for (i <- 0 until 15) {
          W(i) := W(i + 1)
        }
        W(15) := w_t

        h := active_g
        g := active_f
        f := active_e
        e := next_e
        d := active_c
        c := active_b
        b := active_a
        a := next_a

        /*
        // Fault injection point for testing --- IGNORE ---
        when(io.debug_injectFault) {
          a := ~next_a 
        }.otherwise {
          a := next_a
        }
        */

        round := next_round
        when(next_round === 64.U) {
          state := s_update
        }
      }.otherwise {
        for (i <- 0 until 15) {
          dummy_W(i) := dummy_W(i + 1)
        }
        dummy_W(15) := w_t

        dummy_h := active_g
        dummy_g := active_f
        dummy_f := active_e
        dummy_e := next_e
        dummy_d := active_c
        dummy_c := active_b
        dummy_b := active_a
        dummy_a := next_a

        dummy_round := Mux(next_round === 64.U, 0.U, next_round)
      }
    }

    is(s_update) {
      H(0) := H(0) + a
      H(1) := H(1) + b
      H(2) := H(2) + c
      H(3) := H(3) + d
      H(4) := H(4) + e
      H(5) := H(5) + f
      H(6) := H(6) + g
      H(7) := H(7) + h

      when(isLastBlock) {
        state := s_done
      }.otherwise {
        state := s_idle
      }
    }

    is(s_done) {
      when(io.out.ready) {
        for(i <- 0 until 8) {
          H(i) := H_INIT(i)
        }
        state := s_idle
      }
    }
  }
}