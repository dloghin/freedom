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

        println("big_frac=" + big_fraction)

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
                    println("before fraction=" + big_fraction.toString())
                    bitNplusOne = (big_fraction >>> (big_fraction_size-1-fraction_size))&1
                    bitsMore = (big_fraction & ((1<<(big_fraction_size-1-fraction_size))-1))
                    println("before bitsmore=" + bitsMore.toString())
                    if((big_fraction & ((1<<(big_fraction_size-1-fraction_size))-1)) > 0 ) {
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
        println("fraction=" + out_posit.fraction)
        return_value = PositTestDecodeEncode.encode(out_posit, size, max_exponent_size)
        return return_value
    }

    def reverse_bits(v: Int, n: Int): Int = {
        var r = 0
        var i = 0
        var vv = v

        while ( {
            i < n
        }) {
            r |= ((~(v & 1)) & 0x1) << i
            vv = vv >> 1

            i += 1
        }
        r
    }

    def int_IntToPositV2(a : Int, n: Int, es : Int) : Int = {

        if (a == 0)
            return 0

        // x - temporary posit, xx - final posit
        var x = 0
        var xx = 0
        var best = 0
        var sign = 1
        // ival - initial value (save it)
        var ival = a
        var aa = a

        if (aa < 0.0) {
            sign = -1;
            aa = -aa;
        }

        // exp - exponent
        var exp = (scala.math.log(aa)/scala.math.log(2)).toInt;
        // exp_val - the entire value of the exponent (2^exp)
        var exp_val = (1<<es);
        // e - value of exponent field in posit
        var e = exp % exp_val;
        // k - value of regime field in posit
        println("exp " + exp)
        var k = (exp / exp_val).toInt;
        if (exp < 0)
            k = k - 1
        // bit index in posit representation
        var bit_index = 0;
        // based on http://www.johngustafson.net/pdfs/BeatingFloatingPoint.pdf
        k match {
            case -4 => {
                bit_index = n - 5;
            }
            case -3 => {
                bit_index = n - 5;
                x |= (0x1 << bit_index);
            }
            case -2 => {
                bit_index = n - 4;
                x |= (0x1 << bit_index);
            }
            case -1 => {
                bit_index = n - 3;
                x |= (0x1 << bit_index);
            }
            case 0 => {
                bit_index = n - 3;
                x |= (0x2 << bit_index);
            }
            case 1 => {
                bit_index = n - 4;
                x |= (0x6 << bit_index);
            }
            case 2 => {
                bit_index = n - 5;
                x |= (0xE << bit_index);
            }
            case 3 => {
                bit_index = n - 5;
                x |= (0xF << bit_index);
            }
        }
        if ( k < -4 || k > 3) {
            println("Error: unsupported value of k: " + k)
            return 0;
        }

        if (bit_index - es < 0) {
              println("Error: no space left for the exponent.");
            return 0;
        }

        // put the exponent
        x = x | ((e & (exp_val - 1)) << (bit_index - es));
        var frac = ((aa / scala.math.pow(2.0, exp) - 1.0) * (1 << (bit_index - es))).toInt;
        // put the fraction
        x = x | (frac & ((1 << (bit_index - es)) - 1));

        // if the value is negative, take 2's complement
        if (sign == -1) {
            xx = x-1;
            xx = reverse_bits(xx, n);
        }
        else {
            xx = x;
        }
        best = xx;

        // perform tuning by going up and down on the fraction value
       println("Info: posit value: " + best);

        return best;
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
    for (index <- 0 until 1024) {
        poke(dut.io.i_bits, index)
        step(1)
        // aux = PositTestConv.int_PositToInt(index, 32, 3)
        println("index is: " + index.toString)
        println("exp is: " + dut.io.debug_1)
        // println("frac is: " + dut.io.debug_2.asUInt().intValue())
        expect(dut.io.o_bits, aux)
    }
    /**/
}

object TesterPositToIntPosit extends App {
    chisel3.iotesters.Driver(() => new PositPositInt(1, 8)) { c => new TesterPositToIntPosit(c) }
}

class TesterIntToPositPosit(dut : PositIntPosit) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    for (index <- 1 until 10) {
        println("index is: " + index.toString)
        poke(dut.io.i_bits, index)
        step(1)
        // aux = PositTestConv.int_IntToPositV2(index, 8, 1)
        // aux = PositTestConv.int_IntToPositV2(index, 16, 2)
        aux = PositTestConv.int_IntToPositV2(index, 32, 3)
        // println("fraction: " + dut.io.o_posit.asUInt().toString)
        // println("exp is: " + dut.io.o_posit.fraction.litValue())
        // println("frac is: " + dut.io.debug_2)
        expect(dut.io.o_bits, aux)
        // expect(dut.io.o_posit.regime_size, 2)
        val exp = (scala.math.log(index)/scala.math.log(2)).toInt
        var frac = 0
        if (exp > 0)
            frac = index % (1 << exp)
        expect(dut.io.debug_1, 0)
        expect(dut.io.debug_2, 0)
    }
    /**/
}

object TesterIntToPositPosit extends App {
    // chisel3.iotesters.Driver(() => new PositIntPosit(1, 8)) { c => new TesterIntToPositPosit(c) }
    // chisel3.iotesters.Driver(() => new PositIntPosit(2, 16)) { c => new TesterIntToPositPosit(c) }
    chisel3.iotesters.Driver(() => new PositIntPosit(3, 32)) { c => new TesterIntToPositPosit(c) }
}
