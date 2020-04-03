package andled

import chisel3._
import chisel3.Driver
import chisel3.util._


class ANDLED extends Module {
  val io = IO(new Bundle {
    val value1        = Input(Bits(1.W))
    val value2        = Input(Bits(1.W))
    val sel           = Input(Bits(2.W))
    val output        = Output(Bits(1.W))
  })

  val out1  = io.value1 | io.value2
  val out2  = io.value1 & io.value2
  val out3  = io.value1 ^ io.value2
  val out4  = io.value1

  io.output := Mux(io.sel(1), Mux(io.sel(0), out4, out3), Mux(io.sel(0), out2, out1))
}

class Posit32 () extends Bundle {
    val sign = Bool()
    val regime = SInt(32.W)
    val exponent = UInt(32.W)
    val fraction = UInt(32.W)
    val regime_size = UInt(5.W)
    val exponent_size = UInt(5.W)
    val fraction_size = UInt(5.W)
}

class DecodePosit32() extends Module {
    val io = IO(new Bundle {
        val i_bits        = Input(Bits(32.W))
        val o_posit       = Output(new Posit32())
    })

    io.o_posit.sign := io.i_bits(31)
    val tmp = Wire(UInt(31.W))
    
    io.o_posit.exponent_size := 3.U

    tmp := Mux(io.i_bits(31), ~io.i_bits(30,0) + 1.U, io.i_bits(30,0))
    /*
    when(sign === 1.U) {
        tmp := ~io.i_bits(30,0) + 1.U
    }.otherwise {
        tmp := io.i_bits(30,0)
    }
    */
    val regime_size = Wire(UInt(31.W))
    regime_size := Mux(tmp(30), PriorityEncoder(Reverse(~tmp)), PriorityEncoder(Reverse(tmp)))
    io.o_posit.regime := Mux(tmp(30), regime_size.zext - 1.S, -regime_size.zext)
    /*
    regime_size := 0.U
    io.o_posit.regime := 0.S
    when(tmp(30)) {
        regime_size := PriorityEncoder(Reverse(~tmp))
        io.o_posit.regime := regime_size.zext - 1.S
    } .otherwise {
        regime_size := PriorityEncoder(Reverse(tmp))
        io.o_posit.regime := -regime_size.zext
    }
    */
    io.o_posit.regime_size := regime_size + 1.U

    val fraction_size = Wire(SInt(32.W))
    fraction_size := 32.S - 1.S - io.o_posit.regime_size.zext - io.o_posit.exponent_size.zext
    io.o_posit.fraction_size := Mux(fraction_size <= 0.S, 0.U, 32.U - 1.U - io.o_posit.regime_size - io.o_posit.exponent_size)
    /*
    when(fraction_size <= 0.S) {
        io.o_posit.fraction_size := 0.U
    }.otherwise {
        io.o_posit.fraction_size := 32.U - 1.U - io.o_posit.regime_size - io.o_posit.exponent_size
    }
    */

    val exponent_last_index = Wire(SInt(32.W))
    val exponent_first_index = Wire(SInt(32.W))
    val u_exponent_last_index = Wire(UInt(32.W))
    val u_exponent_first_index = Wire(UInt(32.W))
    exponent_last_index := 31.S-regime_size.zext-5.S
    exponent_first_index := 31.S-regime_size.zext-2.S


    u_exponent_last_index := Mux(exponent_last_index <= 0.S, 0.U, 31.U-regime_size-5.U)
    u_exponent_first_index := Mux(exponent_first_index <= 0.S, 0.U, 31.U-regime_size-2.U)
    /*
    u_exponent_last_index := 0.U
    u_exponent_first_index := 0.U

    when(exponent_last_index <= 0.S) {
        u_exponent_last_index := 0.U
    }.otherwise {
        u_exponent_last_index := 31.U-regime_size-5.U
    }

    when(exponent_first_index <= 0.S) {
        u_exponent_first_index := 0.U
    }.otherwise {
        u_exponent_first_index := 31.U-regime_size-2.U
    }
    */

    //io.o_posit.exponent := (Reverse((Reverse(tmp) >> (31.U - u_exponent_first_index - 1.U)) & "hff_ff_ff_ff".U) >> (32.U + u_exponent_last_index - u_exponent_first_index)) & "hff_ff_ff_ff".U
    io.o_posit.exponent := (tmp >> (u_exponent_last_index + 1.U)) &
                           ("hff_ff_ff_ff".U >> (u_exponent_last_index + 3.U + regime_size))
    //io.o_posit.fraction := ("hff_ff_ff_ff".U >> (31.U - u_exponent_last_index) & tmp)
    io.o_posit.fraction := ("hff_ff_ff_ff".U >> (regime_size+5.U) & tmp)
}


object ANDLED extends App {
  chisel3.Driver.execute(Array[String](), () => new ANDLED())
}


object DecodePosit32 extends App {
  chisel3.Driver.execute(Array[String](), () => new DecodePosit32())
}