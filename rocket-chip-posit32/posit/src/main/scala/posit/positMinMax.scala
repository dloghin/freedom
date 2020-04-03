package posit


import chisel3._
import chisel3.Driver
import chisel3.util._


class PositMin(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits_1        = Input(Bits(size.W))
        val i_bits_2        = Input(Bits(size.W))
        val o_bits        = Output(Bits(size.W))
    })

    io.o_bits := 0.U
    when( (io.i_bits_1(size-1) ^ io.i_bits_2(size-1)) === 1.U ) {
        when(io.i_bits_1(size-1) === 1.U) {
            io.o_bits := io.i_bits_1
        } .otherwise {
            io.o_bits := io.i_bits_2
        }
    } .otherwise {
        when(io.i_bits_1(size-1) === 1.U) {
            when(io.i_bits_1 > io.i_bits_2) {
                io.o_bits := io.i_bits_1
            } .otherwise {
                io.o_bits := io.i_bits_2
            }
        } .otherwise {
            when(io.i_bits_1 > io.i_bits_2) {
                io.o_bits := io.i_bits_2
            } .otherwise {
                io.o_bits := io.i_bits_1
            }
        }
    }
}

class PositMax(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits_1        = Input(Bits(size.W))
        val i_bits_2        = Input(Bits(size.W))
        val o_bits        = Output(Bits(size.W))
    })

    io.o_bits := 0.U
    when( (io.i_bits_1(size-1) ^ io.i_bits_2(size-1)) === 1.U ) {
        when(io.i_bits_1(size-1) === 1.U) {
            io.o_bits := io.i_bits_2
        } .otherwise {
            io.o_bits := io.i_bits_1
        }
    } .otherwise {
        when(io.i_bits_1(size-1) === 1.U) {
            when(io.i_bits_1 > io.i_bits_2) {
                io.o_bits := io.i_bits_2
            } .otherwise {
                io.o_bits := io.i_bits_1
            }
        } .otherwise {
            when(io.i_bits_1 > io.i_bits_2) {
                io.o_bits := io.i_bits_1
            } .otherwise {
                io.o_bits := io.i_bits_2
            }
        }
    }
}

/*
class PositTemp(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits_1        = Input(Bits(size.W))
        val i_bits_2        = Input(Bits(size.W))
        val o_bits        = Output(Bits(size.W))
        val i_posit_1     = Output(new Posit(es, size))
        val i_posit_2     = Output(new Posit(es, size))
        val o_posit       = Output(new Posit(es, size))
        val debug_1       = Output(Bits((2*size).W))
        val debug_2       = Output(Bits((2*size).W))
    })


    val decoder_1 = Module(new DecodePosit(es, size))
    val decoder_2 = Module(new DecodePosit(es, size))
    val encoder = Module(new EncodePosit(es, size))
    val encode_bits = Wire(UInt(size.W))
    decoder_1.io.i_bits := io.i_bits_1
    io.i_posit_1 := decoder_1.io.o_posit
    decoder_2.io.i_bits := io.i_bits_2
    io.i_posit_2 := decoder_2.io.o_posit
    encoder.io.i_posit := io.o_posit
    encode_bits := encoder.io.o_bits
    //max exponent size
    io.o_posit.max_exponent_size := io.i_posit_1.max_exponent_size
    //sign
    //io.o_posit.sign := io.i_posit_1.sign ^ io.i_posit_2.sign



}
*/