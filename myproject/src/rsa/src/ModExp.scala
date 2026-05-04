package rsa.src

import chisel3._
import chisel3.util._

class ModExpInput(val keyBits: Int) extends Bundle {
  val base = UInt(keyBits.W) 
  val exp  = UInt(keyBits.W)
  val n    = UInt(keyBits.W)
  val r2   = UInt(keyBits.W)      
}

class ModExp(val keyBits: Int) extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new ModExpInput(keyBits)))
    val out = Decoupled(UInt(keyBits.W))
  })
  
  val sIdle :: sEnterBase :: sWaitBase :: sEnterRes :: sWaitRes :: sSquare :: sWaitSquare :: sMultiply :: sWaitMultiply :: sExit :: sWaitExit :: sDone :: Nil = Enum(12)
  val state = RegInit(sIdle)

  val baseReg = Reg(UInt(keyBits.W))
  val expReg  = Reg(UInt(keyBits.W))
  val nReg    = Reg(UInt(keyBits.W))
  val r2Reg   = Reg(UInt(keyBits.W))
  
  val montBase = Reg(UInt(keyBits.W)) 
  val montRes  = Reg(UInt(keyBits.W)) 
  val finalRes = Reg(UInt(keyBits.W)) 

  val bitCounter = Reg(UInt(log2Ceil(keyBits).W))

  val montMult = Module(new MontgomeryMultiplier(keyBits))

  montMult.io.in.valid := false.B
  montMult.io.in.bits.a := 0.U
  montMult.io.in.bits.b := 0.U
  montMult.io.in.bits.m := nReg
  montMult.io.out.ready := true.B 

  io.in.ready := (state === sIdle)
  io.out.valid := (state === sDone)
  io.out.bits := finalRes

  switch(state) {
    is(sIdle) {
      when(io.in.fire) {
        baseReg := io.in.bits.base
        expReg  := io.in.bits.exp
        nReg    := io.in.bits.n
        r2Reg   := io.in.bits.r2
        bitCounter := (keyBits - 1).U
        state   := sEnterBase
      }
    }

    is(sEnterBase) {
      montMult.io.in.valid := true.B
      montMult.io.in.bits.a := baseReg
      montMult.io.in.bits.b := r2Reg
      when(montMult.io.in.fire) { state := sWaitBase }
    }
    is(sWaitBase) {
      when(montMult.io.out.fire) {
        montBase := montMult.io.out.bits
        state := sEnterRes
      }
    }

    is(sEnterRes) {
      montMult.io.in.valid := true.B
      montMult.io.in.bits.a := 1.U
      montMult.io.in.bits.b := r2Reg 
      when(montMult.io.in.fire) { state := sWaitRes }
    }
    is(sWaitRes) {
      when(montMult.io.out.fire) {
        montRes := montMult.io.out.bits
        state := sSquare
      }
    }

    is(sSquare) {
      montMult.io.in.valid := true.B
      montMult.io.in.bits.a := montRes
      montMult.io.in.bits.b := montRes
      when(montMult.io.in.fire) { state := sWaitSquare }
    }
    is(sWaitSquare) {
      when(montMult.io.out.fire) {
        montRes := montMult.io.out.bits

        when(expReg(bitCounter) === 1.U) {
          state := sMultiply 
        } .otherwise {
          when(bitCounter === 0.U) {
            state := sExit
          } .otherwise {
            bitCounter := bitCounter - 1.U
            state := sSquare
          }
        }
      }
    }

    is(sMultiply) {
      montMult.io.in.valid := true.B
      montMult.io.in.bits.a := montRes
      montMult.io.in.bits.b := montBase
      when(montMult.io.in.fire) { state := sWaitMultiply }
    }
    is(sWaitMultiply) {
      when(montMult.io.out.fire) {
        montRes := montMult.io.out.bits
        
        when(bitCounter === 0.U) {
          state := sExit
        } .otherwise {
          bitCounter := bitCounter - 1.U
          state := sSquare
        }
      }
    }

    is(sExit) {
      montMult.io.in.valid := true.B
      montMult.io.in.bits.a := montRes
      montMult.io.in.bits.b := 1.U 
      when(montMult.io.in.fire) { state := sWaitExit }
    }
    is(sWaitExit) {
      when(montMult.io.out.fire) {
        finalRes := montMult.io.out.bits
        state := sDone
      }
    }

    is(sDone) {
      when(io.out.fire) { 
        state := sIdle 
      }
    }
  }
}