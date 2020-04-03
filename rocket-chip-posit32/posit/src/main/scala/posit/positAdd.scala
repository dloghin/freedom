package posit


import chisel3._
import chisel3.Driver
import chisel3.util._



class PositAdd(es: Int, size : Int) extends Module {
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
    when(io.i_bits_1(size-1) === 0.U) {
        when(io.i_bits_1 >= io.i_bits_2) {
            decoder_1.io.i_bits := io.i_bits_1
            io.i_posit_1 := decoder_1.io.o_posit
            decoder_2.io.i_bits := io.i_bits_2
            io.i_posit_2 := decoder_2.io.o_posit
        } .otherwise {
            decoder_1.io.i_bits := io.i_bits_2
            io.i_posit_1 := decoder_1.io.o_posit
            decoder_2.io.i_bits := io.i_bits_1
            io.i_posit_2 := decoder_2.io.o_posit
        }
    } .otherwise {
        when(io.i_bits_1 >= io.i_bits_2) {
            decoder_1.io.i_bits := io.i_bits_2
            io.i_posit_1 := decoder_1.io.o_posit
            decoder_2.io.i_bits := io.i_bits_1
            io.i_posit_2 := decoder_2.io.o_posit
        } .otherwise {
            decoder_1.io.i_bits := io.i_bits_1
            io.i_posit_1 := decoder_1.io.o_posit
            decoder_2.io.i_bits := io.i_bits_2
            io.i_posit_2 := decoder_2.io.o_posit
        }
    }

    encoder.io.i_posit := io.o_posit
    encode_bits := encoder.io.o_bits
    //max exponent size
    io.o_posit.max_exponent_size := io.i_posit_1.max_exponent_size
    //sign
    //io.o_posit.sign := io.i_posit_1.sign ^ io.i_posit_2.sign

    //
    val fraction_1 = Wire(UInt((2*size).W))
    val fraction_2 = Wire(UInt((2*size).W))
    val fraction_2_aux = Wire(UInt((2*size).W))
    val shiftRight =  Wire(UInt(size.W))
    val shiftRightS =  Wire(SInt(size.W))
    shiftRightS := ((io.i_posit_1.regime - io.i_posit_2.regime) << io.i_posit_1.max_exponent_size) + (io.i_posit_1.exponent.zext - io.i_posit_2.exponent.zext)
    shiftRight := Mux(shiftRightS <= 0.S,
                        0.U,
                        shiftRightS.asUInt)
    //val fraction = Wire(UInt(size.W))
    val fraction_1_result = Wire(UInt((2*size).W))
    val fraction_2_result = Wire(UInt((2*size).W))
    val fraction_to_exponent = Wire(UInt(size.W))
    fraction_1 := ((1.U << (size.U-2.U)) | (io.i_posit_1.fraction << (size.U-2.U-io.i_posit_1.fraction_size))) << size
    fraction_2 := ((1.U << (size.U-2.U)) | (io.i_posit_2.fraction << (size.U-2.U-io.i_posit_2.fraction_size))) << size
    fraction_2_aux := fraction_2 >> shiftRight
    fraction_1_result := fraction_1 + fraction_2_aux
    fraction_to_exponent := fraction_1_result(2*size-1)
    //Poate sa aiba doar unul in plus deoarece 1.x * 1.x < 2 *2 = 4 => (1.x * 1.x) / 2 < 2
    fraction_2_result := Mux(fraction_to_exponent === 0.U,
                            fraction_1_result & ~(1.U << (2*size-2)),
                            (fraction_1_result >> 1) & ~(1.U << (2*size-2)) )
    val exponent_1_result = Wire(UInt(size.W))
    val exponent_2_result = Wire(UInt(size.W))
    val exponent_to_regime = Wire(UInt(size.W))
    exponent_1_result := io.i_posit_1.exponent + fraction_to_exponent
    exponent_to_regime := exponent_1_result >> io.i_posit_1.max_exponent_size
    exponent_2_result := Mux(exponent_to_regime > 0.U,
                            exponent_1_result & (Fill(size, 1.U(1.W))>> (size.U - io.i_posit_1.max_exponent_size)),
                            exponent_1_result)
    val regime_1_result = Wire(SInt((size+2).W))
    regime_1_result := io.i_posit_1.regime + exponent_to_regime.zext
    val regime_size = Wire(UInt(size.W))
    regime_size := Mux(regime_1_result >= 0.S, regime_1_result.asUInt + 2.U, (-regime_1_result).asUInt + 1.U)
    

    /*
    calculate the number of fraction bits
    size - 1 (sign bit) - regime_size - max_exponent_size
    if the value is less than zero or zero than is zero
    */
    val posible_fraction_size = Wire(SInt((log2Ceil(size)+1).W))
    val fraction_size = Wire(UInt((log2Ceil(size)).W))
    posible_fraction_size := size.S - 1.S - io.o_posit.regime_size.zext - io.o_posit.max_exponent_size.zext
    fraction_size := Mux(posible_fraction_size <= 0.S,
                        0.U,
                        size.U - 1.U - io.o_posit.regime_size - io.o_posit.max_exponent_size)
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
    val exponent_size = Wire(UInt((log2Ceil(size)).W))
    exponent_size := Mux(posible_exponent_size_2 <= 0.S,
                        0.U,
                        posible_exponent_size_2.asUInt)
    val bit_nplus1 = Wire(UInt(1.W))
    val bits_more = Wire(UInt(1.W))
    bit_nplus1 := 0.U
    bits_more := 0.U
    val aux_1 = Wire(UInt(size.W))
    val aux_2 = Wire(UInt(size.W))
    aux_1 := 0.U
    aux_2 := 0.U
    io.o_posit.special_number := 0.U
    io.o_posit.regime := 0.S
    io.o_posit.exponent := 0.U
    io.o_posit.fraction := 0.U
    io.o_posit.exponent_size := 0.U
    io.o_posit.fraction_size := 0.U
    io.o_posit.regime_size := 0.U
    io.o_posit.sign := 0.U

    when (io.i_posit_1.special_number) {
        when (io.i_posit_1.sign) {
            io.o_posit.sign := 1.U
            io.o_posit.special_number := 1.U
            io.o_posit.regime := (-(size-2)).S
            io.o_posit.exponent := 0.U
            io.o_posit.fraction := 0.U
            io.o_posit.exponent_size := 0.U
            io.o_posit.fraction_size := 0.U
            io.o_posit.regime_size := 0.U
        } .otherwise {
            io.o_posit.sign := io.i_posit_2.sign
            io.o_posit.special_number := io.i_posit_2.special_number
            io.o_posit.regime := io.i_posit_2.regime
            io.o_posit.exponent := io.i_posit_2.exponent
            io.o_posit.fraction := io.i_posit_2.fraction
            io.o_posit.exponent_size := io.i_posit_2.exponent_size
            io.o_posit.fraction_size := io.i_posit_2.fraction_size
            io.o_posit.regime_size := io.i_posit_2.regime_size
        }
    } .elsewhen (io.i_posit_2.special_number) {
        when (io.i_posit_2.sign) {
            io.o_posit.sign := 1.U
            io.o_posit.special_number := 1.U
            io.o_posit.regime := (-(size-2)).S
            io.o_posit.exponent := 0.U
            io.o_posit.fraction := 0.U
            io.o_posit.exponent_size := 0.U
            io.o_posit.fraction_size := 0.U
            io.o_posit.regime_size := 0.U
        } .otherwise {
            io.o_posit.sign := io.i_posit_1.sign
            io.o_posit.special_number := io.i_posit_1.special_number
            io.o_posit.regime := io.i_posit_1.regime
            io.o_posit.exponent := io.i_posit_1.exponent
            io.o_posit.fraction := io.i_posit_1.fraction
            io.o_posit.exponent_size := io.i_posit_1.exponent_size
            io.o_posit.fraction_size := io.i_posit_1.fraction_size
            io.o_posit.regime_size := io.i_posit_1.regime_size
        }
    } .otherwise {
        //io.o_posit.sign := io.i_posit_1.sign ^ io.i_posit_2.sign
        io.o_posit.sign := 0.U
        //maxpos
        when(regime_1_result >= (size.S - 2.S)) {
            io.o_posit.regime := ((size-2)).S
            io.o_posit.exponent := 0.U
            io.o_posit.fraction := 0.U
            io.o_posit.exponent_size := 0.U
            io.o_posit.fraction_size := 0.U
            io.o_posit.regime_size := (size-1).U
        } .elsewhen (regime_1_result <= -(size.S - 2.S)) {
            io.o_posit.regime := (-(size-2)).S
            io.o_posit.exponent := 0.U
            io.o_posit.fraction := 0.U
            io.o_posit.exponent_size := 0.U
            io.o_posit.fraction_size := 0.U
            io.o_posit.regime_size := (size-1).U
        } .otherwise {
            io.o_posit.regime_size := regime_size
            io.o_posit.regime := regime_1_result

            when(io.o_posit.max_exponent_size - exponent_size >= 2.U) {
                aux_1 := Cat(0.U, Fill((size-1), 1.U)) >> (size.U - 1.U - io.o_posit.max_exponent_size + exponent_size)
                aux_2 := exponent_2_result & aux_1
                bit_nplus1 := (exponent_2_result >> (io.o_posit.max_exponent_size - exponent_size - 1.U)) & 1.U
                bits_more := (~( (exponent_2_result & (aux_1 >> 1)) === 0.U) & 1.U) | (~(fraction_2_result === 0.U) & 1.U)
                io.o_posit.exponent := (exponent_2_result >> (io.o_posit.max_exponent_size - exponent_size))
                io.o_posit.fraction := 0.U
                io.o_posit.exponent_size := exponent_size
                io.o_posit.fraction_size := 0.U
            } .elsewhen ((io.o_posit.max_exponent_size - exponent_size) === 1.U) {
                bit_nplus1 := exponent_2_result & 1.U
                bits_more := (~(fraction_2_result === 0.U) & 1.U)
                io.o_posit.exponent := exponent_2_result >> 1.U
                io.o_posit.fraction := 0.U
                io.o_posit.exponent_size := exponent_size
                io.o_posit.fraction_size := 0.U
            } .otherwise {
                bit_nplus1 := ~((fraction_2_result & (1.U << ((2*size-3).U - fraction_size))) === 0.U) & 1.U
                bits_more := ~((fraction_2_result & (Cat(0.U, Fill((2*size-1), 1.U)) >> (1.U+fraction_size))) === 0.U) & 1.U
                io.o_posit.exponent := exponent_2_result
                io.o_posit.fraction := fraction_2_result >> ((2*size-2).U - fraction_size)
                io.o_posit.exponent_size := exponent_size
                io.o_posit.fraction_size := fraction_size
            }

        }
    }


    val add_one = Wire(Bool())
    val special_add = Wire(Bool())
    val possible_value = Wire(UInt(size.W))
    possible_value := 0.U
    //add_one := bit_nplus1 | bits_more
    add_one := (bit_nplus1 & bits_more) | (bit_nplus1 & ~bits_more & (encode_bits(0)))
    special_add := io.o_posit.special_number | io.i_posit_1.special_number | io.i_posit_2.special_number
    io.debug_1 := bit_nplus1
    io.debug_2 := bits_more
    when (special_add) {
        io.o_bits := encode_bits
    } .otherwise {
        when (add_one) {
            possible_value := encode_bits + 1.U
        }.otherwise {
            possible_value := encode_bits
        }
        when (io.i_posit_1.sign === 1.U) {
            io.o_bits := (1.U << (size-1)) | (~(possible_value-1.U))
        } .otherwise {
            io.o_bits := possible_value
        }
    }
    /**/


}
