package posit


import chisel3._
import chisel3.Driver
import chisel3.util._

class PositSqrt(es: Int, size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits        = Input(Bits(size.W))
        val i_ready        = Input(Bool())
        val o_bits        = Output(Bits(size.W))
        val i_posit       = Output(new Posit(es, size))
        val o_posit       = Output(new Posit(es, size))
        val debug_1       = Output(Bits((2*size).W))
        val debug_2       = Output(Bits((2*size).W))
        val o_ready        = Output(Bool())
    })

    val decoder = Module(new DecodePosit(es, size))
    decoder.io.i_bits := io.i_bits
    io.i_posit := decoder.io.o_posit
    val encoder = Module(new EncodePosit(es, size))
    val encode_bits = Wire(UInt(size.W))
    encoder.io.i_posit := io.o_posit
    encode_bits := encoder.io.o_bits
    //max exponent size
    val max_exponent_size = Wire(UInt((log2Ceil(size)+2).W))
    max_exponent_size := es.U
    //sign
    val sign = Wire(UInt(1.W))
    sign := io.i_posit.sign

    val partial_regime_1 = Wire(SInt(size.W))
    val regime_to_exponent = Wire(UInt(1.W))
    partial_regime_1 := io.i_posit.regime >> 1
    regime_to_exponent := (io.i_posit.regime & 1.S).asUInt
    val partial_exponent_1 = Wire(UInt(size.W))
    partial_exponent_1 := (io.i_posit.exponent | (regime_to_exponent << max_exponent_size))
    val partial_exponent_2 = Wire(UInt(size.W))
    val exponent_to_fraction = Wire(UInt(1.W))
    partial_exponent_2 := partial_exponent_1 >> 1
    exponent_to_fraction := partial_exponent_1 & 1.U
    val partial_fraction_1 = Wire(UInt(size.W))
    partial_fraction_1 := ((1.U << (size.U-2.U)) | (io.i_posit.fraction << (size.U-2.U-io.i_posit.fraction_size))) << exponent_to_fraction
    //val big_fraction = Wire(UInt((2*size).W))
    //big_fraction := partial_fraction << (size-2).U
    val partial_fraction_2 = Wire(UInt(size.W))
    val sqrt_module = Module(new UIntSqrt((size/2)))
    sqrt_module.io.i_bits := partial_fraction_1
    sqrt_module.io.i_ready := io.i_ready
    partial_fraction_2 := sqrt_module.io.o_bits
    io.o_ready :=  sqrt_module.io.o_ready
    
    val partial_fraction_3 = Wire(UInt(size.W))
    //Poate sa aiba doar unul in plus deoarece 1.x * 2 < 2 * 2 = 4 => sqrt(1.x*2) < 2
    partial_fraction_3 := (partial_fraction_2 << ( ((size-2)/2).U)) & ~((1.U << (size.U-2.U)))
    
    val regime = Wire(SInt(size.W))
    val partial_exponent_3 = Wire(UInt(size.W))
    val exponent = Wire(UInt(size.W))
    partial_exponent_3 := partial_exponent_2
    exponent := partial_exponent_3
    regime := partial_regime_1
    /*
    Size occupied by the regime is the number of the same value bit
    plus one (the bit that has a different value)
    */
    val regime_size = Wire(UInt(size.W))
    regime_size := Mux(regime >= 0.S, regime.asUInt + 2.U, (-regime).asUInt + 1.U)

    /*
    calculate the number of fraction bits
    size - 1 (sign bit) - regime_size - max_exponent_size
    if the value is less than zero or zero than is zero
    */
    val posible_fraction_size = Wire(SInt((log2Ceil(size)+1).W))
    posible_fraction_size := size.S - 1.S - regime_size.zext - max_exponent_size.zext
    val fraction_size = Wire(UInt((log2Ceil(size)+2).W))
    fraction_size := Mux(posible_fraction_size <= 0.S,
                        0.U,
                        size.U - 1.U - regime_size - max_exponent_size)
    /*
    If the fraction size is zero than the fraction value is zero
    else is the the least significant fraction_size bits
    TODO: calculat sqrt si apoi modificat
    */
    val fraction = Wire(UInt(size.W))
    fraction := Mux(fraction_size === 0.U,
                    0.U,
                    (partial_fraction_3 >> ((size - 2).U - fraction_size)))
    /*
    Calculate the possible exponent size
    max(0, min(size - 1 (sign bit) - regime_size, max_exponent_size))
    */
    val posible_exponent_size_1 = Wire(SInt((log2Ceil(size)+1).W))
    posible_exponent_size_1 := size.S - 1.S - regime_size.zext
    val posible_exponent_size_2 = Wire(SInt((log2Ceil(size)+1).W))
    posible_exponent_size_2 := Mux(posible_exponent_size_1 < max_exponent_size.zext,
                                   posible_exponent_size_1,
                                   max_exponent_size.zext)
    val exponent_size = Wire(UInt(size.W))
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
    io.o_posit.max_exponent_size :=  max_exponent_size

    when (io.i_bits === 0.U) {
        io.o_posit.special_number := 1.U
        io.o_posit.regime := (-(size-2)).S
        io.o_posit.exponent := 0.U
        io.o_posit.fraction := 0.U
        io.o_posit.exponent_size := 0.U
        io.o_posit.fraction_size := 0.U
        io.o_posit.regime_size := 0.U
        io.o_posit.sign := 0.U
    } .elsewhen(io.i_bits === (1.U << (size-1))) {
        io.o_posit.special_number := 1.U
        io.o_posit.regime := (-(size-2)).S
        io.o_posit.exponent := 0.U
        io.o_posit.fraction := 0.U
        io.o_posit.exponent_size := 0.U
        io.o_posit.fraction_size := 0.U
        io.o_posit.regime_size := 0.U
        io.o_posit.sign := 1.U
    } .otherwise {
        //io.o_posit.sign := io.i_posit_1.sign ^ io.i_posit_2.sign
        io.o_posit.sign := 0.U
        io.o_posit.special_number := 0.U
        //maxpos
        when(regime >= (size.S - 2.S)) {
            io.o_posit.regime := ((size-2)).S
            io.o_posit.exponent := 0.U
            io.o_posit.fraction := 0.U
            io.o_posit.exponent_size := 0.U
            io.o_posit.fraction_size := 0.U
            io.o_posit.regime_size := (size-1).U
            bit_nplus1 := 0.U
            bits_more := 0.U
        } .otherwise {
            io.o_posit.regime_size := regime_size
            io.o_posit.regime := regime

            when( (max_exponent_size - exponent_size) >= 2.U) {
                aux_1 := Cat(0.U, Fill((size-1), 1.U)) >> (size.U - 1.U - max_exponent_size + exponent_size)
                aux_2 := exponent & aux_1
                bit_nplus1 := ((exponent << 1) >> (max_exponent_size - exponent_size)) & 1.U
                bits_more := (~( (exponent & (aux_1 >> 1)) === 0.U) & 1.U) | (~(partial_fraction_3 === 0.U) & 1.U)
                io.o_posit.exponent := (exponent >> (max_exponent_size - exponent_size))
                io.o_posit.fraction := 0.U
                io.o_posit.exponent_size := exponent_size
                io.o_posit.fraction_size := 0.U
            } .elsewhen ((max_exponent_size - exponent_size) === 1.U) {
                bit_nplus1 := exponent & 1.U
                bits_more := (~(partial_fraction_3 === 0.U) & 1.U)
                io.o_posit.exponent := exponent >> 1.U
                io.o_posit.fraction := 0.U
                io.o_posit.exponent_size := exponent_size
                io.o_posit.fraction_size := 0.U
            } .otherwise {
                bit_nplus1 := ~(((partial_fraction_3 >> ( ((size - 2)).U - 1.U - fraction_size)) & 1.U) === 0.U)
                bits_more := ~((partial_fraction_3 & ((1.U << (((size - 2)).U - 1.U - fraction_size)) - 1.U)) === 0.U)
                io.o_posit.exponent := exponent
                io.o_posit.fraction := fraction
                io.o_posit.exponent_size := exponent_size
                io.o_posit.fraction_size := fraction_size
            }

        }
    }

    val add_one = Wire(Bool())
    val possible_value = Wire(UInt(size.W))
    possible_value := 0.U
    //add_one := bit_nplus1 | bits_more
    add_one := (bit_nplus1 & bits_more) | (bit_nplus1 & ~bits_more & (encode_bits(0)))
    io.debug_1 := (max_exponent_size - exponent_size) >= 2.U
    io.debug_2 := bit_nplus1
    when (io.o_posit.special_number) {
        io.o_bits := encode_bits
    } .otherwise {
        when (add_one) {
            possible_value := encode_bits + 1.U
        }.otherwise {
            possible_value := encode_bits
        }
        when (sign === 1.U) {
            io.o_bits := (1.U << (size-1)) | (~(possible_value-1.U))
        } .otherwise {
            io.o_bits := possible_value
        }
    }
}



class UIntSqrt_old_1(size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits        = Input(Bits((2*size).W))
        val o_bits        = Output(Bits(size.W))
        val o_ready        = Output(Bool())
    })

    val r_sem = RegInit(0.U(1.W))
    val r_result = RegInit(0.U(size.W))
    val r_quo = RegInit(0.U(size.W))
    val r_value = RegInit(0.U(size.W))
    val r_counter = RegInit(0.U(size.W))
    r_counter := r_counter + 1.U
    val w_substract = Wire(UInt(size.W))
    w_substract := Cat(r_result, r_quo) & Fill(size, r_quo)
    val w_q_1_value = Wire(UInt(size.W))
    w_q_1_value := Cat(r_result, 1.U) & Fill(size, 1.U)
    val w_q_2_value = Wire(UInt(size.W))
    w_q_2_value := 0.U

    r_value := ( (r_value - w_substract) << 2) | ((io.i_bits >> ((size-2).U - (r_counter << 1))) & 3.U)
    
    when(w_q_1_value > r_value) {
        r_quo := 0.U
    } .otherwise {
        r_quo := 1.U
    }

    r_sem := r_sem
    when(r_counter >= size.U) {
        when(r_sem === 1.U) {
            r_result := r_result
        } .otherwise {
            r_result := (r_result << 1) | r_quo
            r_sem := 1.U
        }
    } .otherwise {
        r_result := (r_result << 1) + (r_quo << 1)
    }
    io.o_bits := r_result
    io.o_ready :=  r_sem
}


class UIntSqrt_old_2(size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits        = Input(Bits((2*size).W))
        val i_ready        = Input(Bool())
        val o_bits        = Output(Bits(size.W))
        val o_ready        = Output(Bool())
        val o_R        = Output(Bits((size+1).W))
        val o_D        = Output(Bits((2*size).W))
        val o_counter  = Output(Bits(size.W))
        val o_wR1  = Output(Bits((size+1).W))
        val o_wR2_1  = Output(Bits((size+1).W))
        val o_wR2_2  = Output(Bits((size+1).W))
    })

    val r_Q = RegInit(0.U(size.W))
    val r_R = RegInit(0.S((size+1).W))
    val r_D = RegInit(0.U((2*size).W));
    val r_counter = RegInit(0.U(size.W))
    val w_R1_1 = Wire(SInt((size+1).W))
    w_R1_1 := (r_R << 2) | ( (r_D >> (r_counter + r_counter)).zext & 3.S)
    val w_R1_2 = Wire(SInt((size+1).W))
    w_R1_2 := w_R1_1 - ((r_Q.zext << 2) | 1.S)
    val w_R2_1 = Wire(SInt((size+1).W))
    w_R2_1 := (r_R << 2) | ( (r_D >> (r_counter + r_counter)).zext & 3.S)
    val w_R2_2 = Wire(SInt((size+1).W))
    w_R2_2 := w_R2_1 + ((r_Q.zext << 2) | 3.S)
    
    when(io.i_ready) {
        r_D := io.i_bits
        r_counter := (size-1).U
        r_R := 0.S
        r_Q := 0.U
    } .otherwise {
        when(r_R >= 0.S) {
            r_R := w_R1_2
            when(w_R1_2 >= 0.S) {
                r_Q := (r_Q << 1) | 1.U
            } .otherwise {
                r_Q := r_Q << 1
            }
        } .otherwise {
            r_R := w_R2_2
            when(w_R2_2 >= 0.S) {
                r_Q := (r_Q << 1) | 1.U
            } .otherwise {
                r_Q := r_Q << 1
            }
        }
        r_counter := r_counter - 1.U
        //r_D := r_D >> 2
    }

    io.o_bits := r_Q
    io.o_ready :=  (r_counter === 0.U)

    io.o_D := r_D
    io.o_R := r_R.asUInt
    io.o_counter  := r_counter
    io.o_wR1  := w_R1_1.asUInt
    io.o_wR2_1  := w_R1_2.asUInt
    io.o_wR2_2  := w_R2_2.asUInt

}



class UIntSqrt(size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits        = Input(Bits((2*size).W))
        val i_ready        = Input(Bool())
        val o_bits        = Output(Bits(size.W))
        val o_ready        = Output(Bool())
        val o_R        = Output(Bits((size+1).W))
        val o_D        = Output(Bits((2*size).W))
        val o_counter  = Output(Bits(size.W))
        val o_wR1  = Output(Bits((size+1).W))
        val o_wR2_1  = Output(Bits((size+1).W))
        val o_wR2_2  = Output(Bits((size+1).W))
    })

    val r_Q = RegInit(0.U(size.W))
    val r_R = RegInit(0.S((size+2).W))
    val r_D = RegInit(0.U((2*size).W));
    val r_counter = RegInit(0.U(size.W))
    val w_R1_1 = Wire(UInt((size+2).W))
    w_R1_1 := (r_R(size,0) << 2) | Cat(r_D(0), r_D(1))
    val w_R1_2 = Wire(SInt((size+2).W))
    w_R1_2 := w_R1_1.asSInt - ((r_Q.zext << 2) | 1.S)
    val w_R2_1 = Wire(UInt((size+2).W))
    w_R2_1 := (r_R(size,0) << 2) | Cat(r_D(0), r_D(1))
    val w_R2_2 = Wire(SInt((size+2).W))
    w_R2_2 := w_R2_1.asSInt + ((r_Q.zext << 2) | 3.S)
    
    when(io.i_ready) {
        r_D := Reverse(io.i_bits)
        r_counter := 0.U
        r_R := 0.S
        r_Q := 0.U
    } .otherwise {
        when(r_R(size+1) === 0.U) {
            r_R := w_R1_2
            when(w_R1_2(size+1) === 0.U) {
                r_Q := (r_Q << 1) | 1.U
            } .otherwise {
                r_Q := r_Q << 1
            }
        } .otherwise {
            r_R := w_R2_2
            when(w_R2_2(size+1) === 0.U) {
                r_Q := (r_Q << 1) | 1.U
            } .otherwise {
                r_Q := r_Q << 1
            }
        }
        r_counter := r_counter + 1.U
        r_D := r_D >> 2
    }

    io.o_bits := r_Q
    io.o_ready :=  (r_counter === size.U)

    io.o_D := r_D
    io.o_R := r_R.asUInt
    io.o_counter  := r_counter
    io.o_wR1  := w_R1_1
    io.o_wR2_1  := w_R1_2.asUInt
    io.o_wR2_2  := w_R2_2.asUInt

}