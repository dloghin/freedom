package posit


import chisel3._
import chisel3.Driver
import chisel3.util._

/*
Posit foarmat
s_rrrrrr_~r_eeeee_ffffff
sign the sign of the number
k=regime the number of rs
exponent with maximum size exponent size
fraction with size fraction

Posit number = (-1)^sign * 2^((2^exponent_size)^regime) * 2^exponent * (1 + fraction/(2^fraction_size))
*/
class Posit (es: Int, size : Int) extends Bundle {
    val sign = Bool()
    val special_number = Bool()
    val regime = SInt(size.W)
    val exponent = UInt(size.W)
    val fraction = UInt(size.W)
    val regime_size = UInt(log2Ceil(size).W)
    val exponent_size = UInt(log2Ceil(size).W)
    val fraction_size = UInt(log2Ceil(size).W)
    val max_exponent_size = UInt(log2Ceil(size).W)

    override def cloneType = (new Posit(es, size)).asInstanceOf[this.type]
}

/*
From input bits it decode the value of the posit number formula
Posit foarmat
s_rrrrrr_~r_eeeee_ffffff
Posit number = (-1)^sign * 2^((2^exponent_size)^regime) * 2^exponent * (1 + fraction/(2^fraction_size))
*/
class DecodePosit(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits        = Input(Bits(size.W))
        val o_posit       = Output(new Posit(es, size))
    })

    /*
    special number 0x0...0 0x10...0
    */
    io.o_posit.special_number := Mux( (io.i_bits === Fill(size, 0.U(1.W))) ||
                                      (io.i_bits === (1.U(1.W) << (size-1) )),
                                    1.U,
                                    0.U)
    /*
    sign is the MSB
    */
    io.o_posit.sign := io.i_bits(size-1)
    /*
    rest of the bits
    */
    val bits_to_decode = Wire(UInt((size-1).W))
    
    /*
    maximum exponent size
    */
    io.o_posit.max_exponent_size := es.U

    /*
    sign is positive you decode de rest of the bits
    sign is negative you decode de two's complement of the rest of the bits
    two's complement of x is ~x plus 1
    */
    bits_to_decode := Mux(io.o_posit.sign,
                          ~io.i_bits((size-2),0) + 1.U,
                          io.i_bits((size-2),0))
    
    /*
    Find the number of continous zeroes or ones in bits.
    If the first bit is one. Negate all the bits, reverse the bits and find the index of the first one.
    If the first bit is zero. Reverse the bits and find the index of the first one.
    */
    val number_of_same_bit_value = Wire(UInt(log2Ceil(size).W))
    number_of_same_bit_value := Mux(bits_to_decode((size-2)),
                                    PriorityEncoder(Reverse(~bits_to_decode)),
                                    PriorityEncoder(Reverse(bits_to_decode)))
    /*
    If the first bit was one than is a pozitive regime with the value:
    the number of consecutive ones minus one
    If the frist bit was zero than is a negative regime with the value:
    minus the number of consecutive zeros
    TODO: doar daca e număr special
    io.o_posit.regime := Mux(bits_to_decode((size-2)),
                            Mux(bits_to_decode(0)===1.U && number_of_same_bit_value===(size-2).U,
                                number_of_same_bit_value.zext,
                                number_of_same_bit_value.zext - 1.S),
                             Mux(bits_to_decode(0)===0.U && number_of_same_bit_value===(size-2).U,
                             -number_of_same_bit_value.zext - 1.S,
                             -number_of_same_bit_value.zext))
    */
    io.o_posit.regime := Mux(bits_to_decode((size-2)),
                            Mux(bits_to_decode(0)===1.U && number_of_same_bit_value===(size-2).U,
                                number_of_same_bit_value.zext,
                                number_of_same_bit_value.zext - 1.S),
                             Mux(bits_to_decode(0)===0.U && number_of_same_bit_value===(size-2).U,
                             -number_of_same_bit_value.zext - 1.S,
                             -number_of_same_bit_value.zext))
    /*
    Size occupied by the regime is the number of the same value bit
    plus one (the bit that has a different value)
    */
    io.o_posit.regime_size := number_of_same_bit_value + 1.U

    /*
    calculate the number of fraction bits
    size - 1 (sign bit) - regime_size - max_exponent_size
    if the value is less than zero or zero than is zero
    */
    val posible_fraction_size = Wire(SInt((log2Ceil(size)+1).W))
    posible_fraction_size := size.S - 1.S - io.o_posit.regime_size.zext - io.o_posit.max_exponent_size.zext
    io.o_posit.fraction_size := Mux(posible_fraction_size <= 0.S,
                                    0.U,
                                    size.U - 1.U - io.o_posit.regime_size - io.o_posit.max_exponent_size)
    /*
    If the fraction size is zero than the fraction value is zero
    else is the the least significant fraction_size bits
    */
    io.o_posit.fraction := Mux(io.o_posit.fraction_size === 0.U,
                               0.U,
                               ( Fill(size, 1.U(1.W)) >> (size.U-io.o_posit.fraction_size) & bits_to_decode))
    /*
    Calculate the possible exponent size
    max(0, min(size - 1 (sign bit) - regime_size, max_exponent_size))
    */
    val posible_exponent_size_1 = Wire(SInt((log2Ceil(size)+1).W))
    posible_exponent_size_1 := size.S - 1.S - io.o_posit.regime_size.zext
    val posible_exponent_size_2 = Wire(SInt((log2Ceil(size)+1).W))
    posible_exponent_size_2 := Mux(posible_exponent_size_1 < io.o_posit.max_exponent_size.zext,
                                   posible_exponent_size_1,
                                   io.o_posit.max_exponent_size.zext)
    io.o_posit.exponent_size := Mux(posible_exponent_size_2 <= 0.S,
                                    0.U,
                                    posible_exponent_size_2.asUInt)

    /*
    If the thexponent size is zero than the exponent is zero,
    otherwise we sift right with fraction_size to eliminate the fraction bits
    and with a mask only for exponent_size
    */
    io.o_posit.exponent := Mux(io.o_posit.exponent_size === 0.U,
                               0.U,
                               (bits_to_decode >> (io.o_posit.fraction_size)) &
                               (Fill(size, 1.U(1.W))>> (size.U - io.o_posit.exponent_size)) )

}

/*
From input posit umber  it encode to bits
Posit foarmat
s_rrrrrr_~r_eeeee_ffffff
Posit number = (-1)^sign * 2^((2^exponent_size)^regime) * 2^exponent * (1 + fraction/(2^fraction_size))
*/
class EncodePosit(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_posit       = Input(new Posit(es, size))
        val o_bits        = Output(Bits(size.W))
    })

    val possible_regime = Wire(UInt(size.W))
    possible_regime := Mux(io.i_posit.regime === (size-2).S,
                            (Fill(size, 1.U(1.W))) & (~(1.U <<(size-1))),
                            Mux(io.i_posit.regime >= 0.S,
                                ( ((Fill(size, 1.U(1.W)) ^ 3.U) << (io.i_posit.exponent_size + io.i_posit.fraction_size)) >> 1.U ) & (~(1.U <<(size-1))),
                                ( (1.U << (io.i_posit.exponent_size + io.i_posit.fraction_size)) )
                               )
                            )
    
                            
    val possible_exponent = Wire(UInt(size.W))
    possible_exponent := Mux(io.i_posit.exponent_size > 0.U,
                                 io.i_posit.exponent << io.i_posit.fraction_size,
                                 0.U)
    val possible_fraction = Wire(UInt(size.W))
    possible_fraction := Mux(io.i_posit.fraction_size > 0.U,
                                 io.i_posit.fraction,
                                 0.U)
    val possible_value = Wire(UInt(size.W))
    possible_value := possible_regime | possible_exponent | possible_fraction
    /**/
    io.o_bits := Mux(io.i_posit.special_number === 1.U,
                  Mux(io.i_posit.sign === 1.U, (1.U(1.W) << (size-1)), Fill(size, 0.U(1.W))),
                  Mux(io.i_posit.sign === 1.U,
                    (io.i_posit.sign << (size-1)) | (~(possible_value-1.U)),
                    possible_value)
                  )
    //*/
    //io.o_bits := possible_regime
}

class DeEnPosit(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits        = Input(Bits(size.W))
        val o_bits        = Output(Bits(size.W))
        val o_posit       = Output(new Posit(es, size))
    })

    val encoder = Module(new EncodePosit(es, size))
    val decoder = Module(new DecodePosit(es, size))

    decoder.io.i_bits := io.i_bits
    encoder.io.i_posit := decoder.io.o_posit
    io.o_posit := decoder.io.o_posit
    io.o_bits := encoder.io.o_bits
}

object DecodePosit extends App {
  chisel3.Driver.execute(Array[String](), () => new DecodePosit(3, 32))
}