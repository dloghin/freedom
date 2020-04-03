package posit


import chisel3._
import chisel3.Driver
import chisel3.util._


class PositPositInt(es: Int, size: Int) extends Module {
  val io = IO(new Bundle {
    val i_bits = Input(Bits(size.W))
    val o_bits = Output(Bits(size.W))
    val o_posit = Output(new Posit(es, size))
    val debug_1 = Output(Bits((2 * size).W))
    val debug_2 = Output(Bits((2 * size).W))
  })


  val decoder = Module(new DecodePosit(es, size))
  decoder.io.i_bits := io.i_bits
  io.o_posit := decoder.io.o_posit
  val max_bit = Wire(Bits((2 * size).W))
  val max_bit_aux = Wire(Bits((log2Ceil(size) + 2).W))
  max_bit := 0.U
  when(io.o_posit.regime >= 0.S) {
    max_bit := ((io.o_posit.regime.asUInt << io.o_posit.max_exponent_size) + io.o_posit.exponent)
  }.otherwise {
    max_bit := 0.U
  }
  val value_1 = Wire(Bits((2 * size).W))
  value_1 := 0.U
  max_bit_aux := max_bit
  when(io.o_posit.regime >= 0.S) {
    value_1 := (1.U << max_bit_aux)
  }.otherwise {
    value_1 := 0.U
  }
  val value_2 = Wire(Bits((2 * size).W))
  //value_2 := (value_1 * ((1.U << io.o_posit.fraction_size) | fraction))
  value_2 := (value_1 * io.o_posit.fraction)
  val bit_nplus1 = Wire(UInt(1.W))
  val bits_more = Wire(UInt(1.W))
  bit_nplus1 := 0.U
  bits_more := 0.U
  when(io.o_posit.fraction_size > 0.U) {
    bit_nplus1 := ~((value_2 & (1.U << (io.o_posit.fraction_size - 1.U))) === 0.U)
    bits_more := ~((value_2 & (((1.U << (io.o_posit.fraction_size - 1.U)) - 1.U))) === 0.U)
  }.otherwise {
    bit_nplus1 := 0.U
    bits_more := 0.U
  }
  val add_one = Wire(Bool())
  //add_one := bit_nplus1 | bits_more
  add_one := (bit_nplus1 & bits_more) | (bit_nplus1 & ~bits_more & (((value_2 >> io.o_posit.fraction_size) | value_1) & 1.U))
  val value_3 = Wire(Bits((2 * size).W))
  value_3 := 0.U
  when(add_one) {
    value_3 := ((value_2 >> io.o_posit.fraction_size) | value_1) + 1.U
  }.otherwise {
    value_3 := (value_2 >> io.o_posit.fraction_size) | value_1
  }

  val value_4 = Wire(Bits((2 * size).W))
  value_4 := 0.U
  io.debug_1 := ((1.U << (size - 1)) - 1.U)
  io.debug_2 := value_3
  when((value_3 > ((1.U << (size - 1)) - 1.U)) || (max_bit > (size - 2).U)) {
    value_4 := ((1.U << (size - 1)) - 1.U)
  }.otherwise {
    value_4 := value_3
  }
  io.o_bits := 0.U
  when(io.o_posit.special_number === 1.U) {
    io.o_bits := 0.U
  }.otherwise {
    when(io.o_posit.sign === 1.U) {
      //TODO: de modificat daca vrem altfeld e rotunjire
      when(value_4 === 0.U) {
        io.o_bits := 0.U
      }.otherwise {
        io.o_bits := (io.o_posit.sign << (size - 1)) | ((~(value_4 - 1.U)) & ((1.U << (size - 1)) - 1.U))
      }
    }.otherwise {
      io.o_bits := value_4 & ((1.U << (size - 1)) - 1.U)
    }
  }
}

class PositIntPosit(es: Int, size: Int) extends Module {
  val io = IO(new Bundle {
    val i_bits = Input(Bits(size.W))
    val o_bits = Output(Bits(size.W))
    val o_posit = Output(new Posit(es, size))
    val debug_1 = Output(Bits((2 * size).W))
    val debug_2 = Output(Bits((2 * size).W))
  })

  val encoder = Module(new EncodePosit(es, size))
  val encode_bits = Wire(UInt(size.W))
  encoder.io.i_posit := io.o_posit
  encode_bits := encoder.io.o_bits
  //max exponent size
  val max_exponent_size = Wire(UInt((log2Ceil(size) + 2).W))
  max_exponent_size := es.U
  //sign
  val sign = Wire(UInt(1.W))
  sign := io.i_bits(size - 1)
  val value = Wire(UInt(size.W))
  when(sign === 1.U) {
    value := (~(io.i_bits) + 1.U) & Cat(0.U, Fill((size - 1), 1.U))
  }.otherwise {
    value := io.i_bits & Cat(0.U, Fill((size - 1), 1.U))
  }

  val exp = Wire(UInt(size.W))
  exp := 0.U
  /*
  when((value & (1.U << 7)) === (1.U << 7)) {
    exp := 7.U
  } otherwise {
    when((value & (1.U << 6)) === (1.U << 6)) {
      exp := 6.U
    } otherwise ({
      when((value & (1.U << 5)) === (1.U << 5)) {
        exp := 5.U
      } otherwise ({
        when((value & (1.U << 4)) === (1.U << 4)) {
          exp := 4.U
        } otherwise ({
          when((value & (1.U << 3)) === (1.U << 3)) {
            exp := 3.U
          } otherwise ({
            when((value & (1.U << 2)) === (1.U << 2)) {
              exp := 2.U
            } otherwise ({
              when((value & (1.U << 1)) === (1.U << 1)) {
                exp := 1.U
              } otherwise ({
                exp := 0.U
              })
            })
          })
        })
      })
    })
  }
  */
  exp := size.U - 1.U - PriorityEncoder(Reverse(value))
  val expShift = Wire(UInt(19.W))

  val frac = Wire(UInt(size.W))
  val final_frac = Wire(UInt(size.W))

  val mask = Wire(UInt(size.W))
  val mask1 = Wire(UInt(size.W))
  mask := Fill(size - 1, 1.U)
  mask1 := mask << 18.U
  // frac := (value & ~(((1.U << (size - 1)) - 1.U) << exp))
  frac := (value & ~(mask << exp))
  /*
  when (exp > 18.U){
    expShift := exp - 18.U
    frac := (value & ~(mask1 << expShift))
  } .otherwise({
    expShift := exp
    frac := (value & ~(mask << expShift))
  })
   */
  val exponent = Wire(UInt(size.W))
  val regime = Wire(UInt(size.W))
  val regime_size = Wire(UInt(size.W))
  exponent := exp % (1.U << es).asUInt()
  expShift := exp - 0.U
  val rn = (exp >> es) + 1.U
  when(exp >= (1.U << es).asUInt()) {
    // regime := Fill(exp.litValue().intValue(), 1.U)
    regime := (1.U << (rn + 1.U)) - 1.U
    regime_size := rn + 1.U
  }.otherwise {
    regime := 2.U
    regime_size := 2.U
  }
  val frac_size = Wire(UInt(size.W))
  frac_size := size.U - 1.U - regime_size - es.U;
  when(frac_size > exp) {
    expShift := frac_size - exp
    final_frac := frac << expShift
  }.otherwise {
    when(frac_size < exp) {
      expShift := exp - frac_size
      final_frac := frac << expShift
    } .otherwise({
      final_frac := frac
    })
  }

  io.o_posit.special_number := 0.U
  io.o_posit.sign := sign
  io.o_posit.regime_size := regime_size
  io.o_posit.regime := regime.asSInt()
  io.o_posit.exponent := exponent
  io.o_posit.fraction := final_frac
  io.o_posit.exponent_size := es.U
  io.o_posit.fraction_size := frac_size
  io.o_posit.max_exponent_size := max_exponent_size
  io.debug_1 := exp
  io.debug_2 := frac

  when(io.i_bits === 0.U) {
    io.o_posit.special_number := 1.U
    io.o_posit.regime := (-(size - 2)).S
    io.o_posit.exponent := 0.U
    io.o_posit.fraction := 0.U
    io.o_posit.exponent_size := 0.U
    io.o_posit.fraction_size := 0.U
    io.o_posit.regime_size := 0.U
    io.o_posit.sign := 0.U
  }

  val possible_value = Wire(UInt(size.W))
  possible_value := 0.U
  when(io.o_posit.special_number) {
    io.o_bits := encode_bits
  }.otherwise {
    possible_value := encode_bits
    when(sign === 1.U) {
      io.o_bits := (1.U << (size - 1)) | (~(possible_value - 1.U))
    }.otherwise {
      io.o_bits := possible_value
    }
  }

  /*
  val index = Wire(UInt((log2Ceil(size)+2).W))
  index := (size-1).U - PriorityEncoder(Reverse(value))
  val regime = Wire(UInt(size.W))
  regime := index >> max_exponent_size
  val exponent = Wire(UInt(size.W))
  exponent := index & ((1.U << max_exponent_size) - 1.U)
  val big_fraction = Wire(UInt((2*size).W))
  big_fraction := value & ((1.U << index) - 1.U)
  val big_fraction_size = Wire(UInt(size.W))
  big_fraction_size := index - 1.U

  /*
  Size occupied by the regime is the number of the same value bit
  plus one (the bit that has a different value)
  */
  val regime_size = Wire(UInt(size.W))
  regime_size := regime + 2.U

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
  */
  val fraction = Wire(UInt(size.W))
  fraction := Mux(fraction_size === 0.U,
                  0.U,
                  big_fraction >> (index-1.U-fraction_size) )
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


  io.o_posit.special_number := 0.U
  io.o_posit.regime := 0.S
  io.o_posit.exponent := 0.U
  io.o_posit.fraction := 0.U
  io.o_posit.exponent_size := 0.U
  io.o_posit.fraction_size := 0.U
  io.o_posit.regime_size := 0.U
  io.o_posit.sign := 0.U
  io.o_posit.max_exponent_size :=  max_exponent_size
  aux_1 := 0.U
  aux_2 := 0.U
  when (io.i_bits === 0.U) {
      io.o_posit.special_number := 1.U
      io.o_posit.regime := (-(size-2)).S
      io.o_posit.exponent := 0.U
      io.o_posit.fraction := 0.U
      io.o_posit.exponent_size := 0.U
      io.o_posit.fraction_size := 0.U
      io.o_posit.regime_size := 0.U
      io.o_posit.sign := 0.U
  } .otherwise {
      //io.o_posit.sign := io.i_posit_1.sign ^ io.i_posit_2.sign
      io.o_posit.sign := 0.U
      io.o_posit.special_number := 0.U
      //maxpos
      when(regime >= ( (size - 2).U)) {
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
          io.o_posit.regime := regime.zext

          when(max_exponent_size - exponent_size >= 2.U) {
              aux_1 := Cat(0.U, Fill((size-1), 1.U)) >> (size.U - 1.U - max_exponent_size + exponent_size)
              aux_2 := exponent & aux_1
              bit_nplus1 := (exponent >> (io.o_posit.max_exponent_size - exponent_size - 1.U)) & 1.U
              bits_more := (~( (exponent & (aux_1 >> 1)) === 0.U) & 1.U) | (~(big_fraction === 0.U) & 1.U)
              io.o_posit.exponent := (exponent >> (io.o_posit.max_exponent_size - exponent_size))
              io.o_posit.fraction := 0.U
              io.o_posit.exponent_size := exponent_size
              io.o_posit.fraction_size := 0.U
          } .elsewhen ((max_exponent_size - exponent_size) === 1.U) {
              bit_nplus1 := exponent & 1.U
              bits_more := (~(big_fraction === 0.U) & 1.U)
              io.o_posit.exponent := exponent >> 1.U
              io.o_posit.fraction := 0.U
              io.o_posit.exponent_size := exponent_size
              io.o_posit.fraction_size := 0.U
          } .otherwise {
              bit_nplus1 := ~(((big_fraction >> (index - 2.U - fraction_size)) & 1.U) === 0.U)
              bits_more := ~((big_fraction & ((1.U << (index - 2.U - fraction_size)) - 1.U)) === 0.U)
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
  io.debug_1 := bit_nplus1
  io.debug_2 := bits_more
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
  */
}
