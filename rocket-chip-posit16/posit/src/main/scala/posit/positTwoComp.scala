package posit


import chisel3._
import chisel3.Driver
import chisel3.util._

class PositTwoComp(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits        = Input(Bits(size.W))
        val o_bits        = Output(Bits(size.W))
    })

    io.o_bits := 0.U
    when(io.i_bits === 0.U) {
        io.o_bits := io.i_bits
    }.otherwise {
        when(io.i_bits(size-1) === 1.U) {
            io.o_bits := (~(io.i_bits - 1.U)) & ((1.U<<(size-1)) - 1.U)
        } .otherwise {
            io.o_bits := (1.U<<(size-1)) | (((~(Cat(0.U(1.W),io.i_bits(size-2, 0)))) + 1.U) & ((1.U<<(size-1)) - 1.U))
        }
    }
}