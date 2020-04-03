package posit


import chisel3._
import chisel3.Driver
import chisel3.util._

class PositSGNJ(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits_1        = Input(Bits(size.W))
        val i_bits_2        = Input(Bits(size.W))
        val o_bits        = Output(Bits(size.W))
    })

    val value = Wire(Bits((size-1).W))
    value := 0.U
    when(io.i_bits_1(size-1) === 1.U) {
        value := (~(io.i_bits_1 - 1.U)) & ((1.U<<(size-1)) - 1.U)
    } .otherwise {
        value := io.i_bits_1(size-2, 0)
    }
    io.o_bits := 0.U
    when(io.i_bits_2(size-1) === 1.U) {
        io.o_bits := (1.U<<(size-1)) | (((~(value)) + 1.U) & ((1.U<<(size-1)) - 1.U))
    } .otherwise {
        io.o_bits := value
    }
}


class PositSGNJN(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits_1        = Input(Bits(size.W))
        val i_bits_2        = Input(Bits(size.W))
        val o_bits        = Output(Bits(size.W))
    })

    val value = Wire(Bits((size-1).W))
    value := 0.U
    when(io.i_bits_1(size-1) === 1.U) {
        value := (~(io.i_bits_1 - 1.U)) & ((1.U<<(size-1)) - 1.U)
    } .otherwise {
        value := io.i_bits_1(size-2, 0)
    }
    io.o_bits := 0.U
    when(io.i_bits_2(size-1) === 1.U) {
        io.o_bits := value
    } .otherwise {
        io.o_bits := (1.U<<(size-1)) | (((~(value)) + 1.U) & ((1.U<<(size-1)) - 1.U))
    }
}


class PositSGNJX(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits_1        = Input(Bits(size.W))
        val i_bits_2        = Input(Bits(size.W))
        val o_bits        = Output(Bits(size.W))
    })

    val value = Wire(Bits((size-1).W))
    value := 0.U
    when(io.i_bits_1(size-1) === 1.U) {
        value := (~(io.i_bits_1 - 1.U)) & ((1.U<<(size-1)) - 1.U)
    } .otherwise {
        value := io.i_bits_1(size-2, 0)
    }
    io.o_bits := 0.U
    when( (io.i_bits_2(size-1) ^ io.i_bits_1(size-1)) === 1.U) {
        io.o_bits := (1.U<<(size-1)) | (((~(value)) + 1.U) & ((1.U<<(size-1)) - 1.U))
    } .otherwise {
        io.o_bits := value
    }
}
