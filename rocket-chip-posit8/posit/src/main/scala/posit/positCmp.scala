package posit


import chisel3._
import chisel3.Driver
import chisel3.util._


class PositLE(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits_1        = Input(Bits(size.W))
        val i_bits_2        = Input(Bits(size.W))
        val o_result        = Output(Bool())
    })

    io.o_result := 0.U
    when( (io.i_bits_1(size-1) ^ io.i_bits_2(size-1)) === 1.U ) {
        when(io.i_bits_1(size-1) === 1.U) {
            io.o_result := 1.U
        } .otherwise {
            io.o_result := 0.U
        }
    } .otherwise {
        when(io.i_bits_1(size-1) === 1.U) {
            when(io.i_bits_1 >= io.i_bits_2) {
                io.o_result := 1.U
            } .otherwise {
                io.o_result := 0.U
            }
        } .otherwise {
            when(io.i_bits_1 > io.i_bits_2) {
                io.o_result := 0.U
            } .otherwise {
                io.o_result := 1.U
            }
        }
    }
}

class PositL(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits_1        = Input(Bits(size.W))
        val i_bits_2        = Input(Bits(size.W))
        val o_result        = Output(Bool())
    })

    io.o_result := 0.U
    when( (io.i_bits_1(size-1) ^ io.i_bits_2(size-1)) === 1.U ) {
        when(io.i_bits_1(size-1) === 1.U) {
            io.o_result := 1.U
        } .otherwise {
            io.o_result := 0.U
        }
    } .otherwise {
        when(io.i_bits_1(size-1) === 1.U) {
            when(io.i_bits_1 > io.i_bits_2) {
                io.o_result := 1.U
            } .otherwise {
                io.o_result := 0.U
            }
        } .otherwise {
            when(io.i_bits_1 >= io.i_bits_2) {
                io.o_result := 0.U
            } .otherwise {
                io.o_result := 1.U
            }
        }
    }
}

class PositE(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits_1        = Input(Bits(size.W))
        val i_bits_2        = Input(Bits(size.W))
        val o_result        = Output(Bool())
    })

    io.o_result := 0.U
    when( io.i_bits_1 === io.i_bits_2 ) {
        io.o_result := 1.U
    } .otherwise {
        io.o_result := 0.U
    }
}