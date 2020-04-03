package posit

import chisel3._
import chisel3.iotesters._

object PositTestConv {
    def int_PositToInt(a : Int, size: Int, max_exponent_size : Int) : Int = {
        var out_posit: TestPosit = new TestPosit(max_exponent_size, size)
        var in_posit: TestPosit = new TestPosit(max_exponent_size, size)
        var return_value: Int = 0
        var fraction_value: Int = 0
        var partial_power: Int = 0
        var maxint_plus = (1<<(size-1))-1
        var maxint_minus = (1<<(size-1))+1
        in_posit = PositTestDecodeEncode.decode(a, size, max_exponent_size)
        if(in_posit.special_number > 0) {
            return 0
        }
        if(in_posit.regime < 0) {
            if(in_posit.sign > 0) {
                //return (1<<(size)) - 1
                return 0
            } else {
                return 0
            }
        }
        println("in_posit.regime is: " + in_posit.regime.toString)
        println("in_posit.exponent is: " + in_posit.exponent.toString)
        println("max_exponent_size is: " + max_exponent_size.toString)
        return_value =  scala.math.pow(scala.math.pow(2,scala.math.pow(2,max_exponent_size).toInt).toInt, in_posit.regime).toInt
        println("return_value is: " + return_value.toString)
        return_value =  return_value * scala.math.pow(2,in_posit.exponent).toInt
        println("return_value is: " + return_value.toString)
        fraction_value =  return_value * in_posit.fraction
        println("fraction_value is: " + fraction_value.toString)
        return_value =  return_value + ((return_value * in_posit.fraction) >> in_posit.fraction_size)
        println("return_value is: " + return_value.toString)
        partial_power = scala.math.pow(2,max_exponent_size).toInt * in_posit.regime
        println("partial_power is: " + partial_power.toString)
        if( (partial_power + in_posit.exponent) > (size-2)) {
            if(in_posit.sign == 1) {
                return maxint_minus
            } else {
                return maxint_plus
            }
        }

        var bitNplusOne: Boolean = false
        var bitsMore: Boolean = false
        if(in_posit.fraction_size > 0) {
            bitNplusOne = !((fraction_value & (1 << (in_posit.fraction_size-1))) == 0)
            bitsMore  = !((fraction_value & ((1 << (in_posit.fraction_size))-1)) == 0)
        }
        if( bitNplusOne || bitsMore) {
            return_value = return_value + 1
        }
        println("return_value is: " + return_value.toString)
        if (in_posit.sign > 0) {
            return_value = (1<<(size-1)) | ((~(return_value-1)) & ((1<<(size-1))-1))
        }
        println("return_value is: " + return_value.toString)
        return return_value
    }

    def int_IntToPosit(a : Int, size: Int, max_exponent_size : Int) : Int = {
        var out_posit: TestPosit = new TestPosit(max_exponent_size, size)
        var return_value: Int = 0
        var value: Int = 0
        if((a>>(size-1)) > 0) {
            value = (~a & ((1<<(size-1))-1)) + 1
        } else {
            value = a & ((1<<(size-1))-1)
        }

        var fraction: Int = 0
        var exponent: Int = 0
        var regime: Int = 0
        var prod_1: Int = 1;
        while(prod_1 < value) {
            prod_1 = prod_1 * scala.math.pow(2,scala.math.pow(2,max_exponent_size).toInt).toInt
            regime = regime + 1
        }
        if(prod_1 == 1) {
            regime = 0
        } else {
            regime = regime - 1
            prod_1 = prod_1 / scala.math.pow(2,scala.math.pow(2,max_exponent_size).toInt).toInt
        }
        var prod_2: Int = prod_1;
        while(prod_2 < value) {
            prod_2 = prod_2 * 2
            exponent = exponent + 1
        }
        if(prod_1 == prod_2) {
            exponent = 0
        } else {
            exponent = exponent - 1
            prod_2 = prod_2 / 2
        }

        var big_fraction = value % prod_2
        var big_fraction_size : Int = scala.math.pow(2,max_exponent_size).toInt * regime + exponent


        var sign: Int = (a>>(size-1))

        if(regime >= size-2) {
            return_value = ((1 << (size-1)) - 1)
            if(sign == 1) {
                return_value = ~(return_value - 1) & ((1 << (size)) - 1)
            }
            return return_value
        }
        if(regime <= -(size-2)) {
            return_value = 1
            if(sign == 1) {
                return_value = ~(return_value - 1) & ((1 << (size)) - 1)
            }
            return return_value
        }

        var regime_size: Int = 0
        if(regime >= 0) {
            regime_size = regime + 2
        } else {
            regime_size = -regime + 1
        }
        var exponent_size: Int = 0
        exponent_size = size - 1 - regime_size
        if(exponent_size < 0) {
            exponent_size = 0
        }
        if(exponent_size > max_exponent_size) {
            exponent_size = max_exponent_size
        }
        var fraction_size: Int = 0
        fraction_size = size - 1 - regime_size - max_exponent_size
        if(fraction_size < 0) {
            fraction_size = 0
        }

        var bitNplusOne: Int = 0
        var bitsMore: Int = 0
        var aux: Int = 0

        out_posit.sign = (a>>(size-1))
        if(value == 0) {
            out_posit.special_number = 1
            out_posit.regime = -(size-1)
            out_posit.regime_size = (size-1)
            out_posit.exponent = 0
            out_posit.exponent_size = 0
            out_posit.fraction = 0
            out_posit.fraction_size = 0
        } else {
            if(max_exponent_size - exponent_size >= 2) {
                bitNplusOne = (exponent & (((1<<(max_exponent_size-exponent_size))-1)))
                println("before bitNplusOne=" + bitNplusOne.toString())
                bitNplusOne = (exponent & (((1<<(max_exponent_size-exponent_size))-1))) >>> (max_exponent_size-exponent_size-1)
                aux = (exponent & (((1<<(max_exponent_size-exponent_size-1))-1)))
                println("before aux=" + aux.toString())
                if(aux > 0 || big_fraction > 0) {
                    bitsMore = 1
                    fraction = 0
                }
                exponent = exponent >>> (max_exponent_size-exponent_size)
            } else {
                if(max_exponent_size - exponent_size == 1) {
                    bitNplusOne = exponent & 1
                    if(big_fraction > 0) {
                        bitsMore = 1
                        fraction = 0
                    }
                    exponent = exponent >> 1
                } else {
                    println("before fraction=" + fraction.toString())
                    bitNplusOne = (big_fraction >>> (big_fraction_size-1-fraction_size))&1
                    bitsMore = (fraction & ((1<<(big_fraction_size-1-fraction_size))-1))
                    println("before bitsmore=" + bitsMore.toString())
                    if((fraction & ((1<<(big_fraction_size-1-fraction_size))-1)) > 0 ) {
                        bitsMore = 1
                    }
                    fraction = big_fraction >>> (big_fraction_size-fraction_size)
                }
            }
            out_posit.special_number = 0
            out_posit.regime = regime
            out_posit.regime_size = regime_size
            out_posit.exponent = exponent
            out_posit.exponent_size = exponent_size
            out_posit.fraction = fraction
            out_posit.fraction_size = fraction_size
        }
        return_value = PositTestDecodeEncode.encode(out_posit, size, max_exponent_size)
        return return_value
    }

}


class TesterPositToIntPosit(dut : PositPositInt) extends PeekPokeTester(dut) {
    var aux: Int = 0;
    /*
    var index: Int = 193
    poke(dut.io.i_bits, index)
    step(1)
    aux = PositTestConv.int_PositToInt(index, 8, 1)
    println("index is: " + index.toString)
    println("debug_1 is: " + peek(dut.io.debug_1).toString)
    println("debug_2 is: " + peek(dut.io.debug_2).toString)
    expect(dut.io.o_bits, aux)
    */
    for (index <- 0 until 256) {
        poke(dut.io.i_bits, index)
        step(1)
        aux = PositTestConv.int_PositToInt(index, 8, 1)
        println("index is: " + index.toString)
        expect(dut.io.o_bits, aux)
    }
    /**/
}

object TesterPositToIntPosit extends App {
    chisel3.iotesters.Driver(() => new PositPositInt(1, 8)) { c => new TesterPositToIntPosit(c) }
}

class TesterIntToPositPosit(dut : PositIntPosit) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    for (index <- 0 until 256) {
        poke(dut.io.i_bits, index)
        step(1)
        aux = PositTestConv.int_IntToPosit(index, 8, 1)
        println("index is: " + index.toString)
        expect(dut.io.o_bits, aux)
    }
    /**/
}

object TesterIntToPositPosit extends App {
    chisel3.iotesters.Driver(() => new PositIntPosit(1, 8)) { c => new TesterIntToPositPosit(c) }
}

