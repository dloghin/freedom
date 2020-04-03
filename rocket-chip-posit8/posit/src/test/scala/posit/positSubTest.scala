package posit

import chisel3._
import chisel3.iotesters._

import java.math.BigInteger

object PositTestSub {
    def int_sub(a : Int, b : Int, size: Int, max_exponent_size : Int) : Int = {
        var posit_1: TestPosit = new TestPosit(max_exponent_size, size)
        var posit_2: TestPosit = new TestPosit(max_exponent_size, size)
        var out_posit: TestPosit = new TestPosit(max_exponent_size, size)
        var initial_sign: Int = 0;
        if((a>>(size-1))==0) {
            if(a >= b) {
                posit_1 = PositTestDecodeEncode.decode(a, size, max_exponent_size)
                posit_2 = PositTestDecodeEncode.decode(b, size, max_exponent_size)
            } else {
                posit_1 = PositTestDecodeEncode.decode(b, size, max_exponent_size)
                posit_2 = PositTestDecodeEncode.decode(a, size, max_exponent_size)
                initial_sign = 1
            }
        } else {
            if(a >= b) {
                posit_1 = PositTestDecodeEncode.decode(b, size, max_exponent_size)
                posit_2 = PositTestDecodeEncode.decode(a, size, max_exponent_size)
                initial_sign = 1
            } else {
                posit_1 = PositTestDecodeEncode.decode(a, size, max_exponent_size)
                posit_2 = PositTestDecodeEncode.decode(b, size, max_exponent_size)
            }
        }
        if(posit_1.special_number==1) {
            if(posit_2.special_number==1) {
                if(posit_1.sign==1) {
                    return PositTestDecodeEncode.encode(posit_1, size, max_exponent_size)
                }
                if(posit_2.sign==1) {
                    return PositTestDecodeEncode.encode(posit_2, size, max_exponent_size)
                }
            }
            if(posit_1.sign==1) {
                return PositTestDecodeEncode.encode(posit_1, size, max_exponent_size)
            } else {
                posit_2.sign = initial_sign ^ posit_2.sign
                return PositTestDecodeEncode.encode(posit_2, size, max_exponent_size)
            }
        } else {
            if(posit_2.special_number==1) {
                if(posit_2.sign==1) {
                    return PositTestDecodeEncode.encode(posit_2, size, max_exponent_size)
                } else {
                    posit_1.sign = initial_sign ^ posit_1.sign
                    return PositTestDecodeEncode.encode(posit_1, size, max_exponent_size)
                }
            }
        }

        var fraction_1: Int = ((1 << (size-2)) + (posit_1.fraction << (size-2-posit_1.fraction_size))) << size
        var fraction_2: Int = ((1 << (size-2)) + (posit_2.fraction << (size-2-posit_2.fraction_size))) << size
        var shiftRight: Int = ((posit_1.regime - posit_2.regime) << max_exponent_size) + (posit_1.exponent - posit_2.exponent)
        println("fraction_2=" + fraction_2.toString())
        println("shiftRight=" + shiftRight.toString())
        if (shiftRight > (2*size-1)) {
            fraction_2 = 0
        } else {
            fraction_2 = fraction_2 >> shiftRight
        }
        var fraction: Int = fraction_1 - fraction_2
        var exponent: Int = posit_1.exponent
        var regime: Int = posit_1.regime
        println("p1 regime=" + posit_1.regime.toString())
        println("p2 regime=" + posit_2.regime.toString())
        println("p1 exponent=" + posit_1.exponent.toString())
        println("p2 exponent=" + posit_2.exponent.toString())
        println("p1 fraction=" + posit_1.fraction.toString())
        println("p2 fraction=" + posit_2.fraction.toString())
        println("p1 fraction_size=" + posit_1.fraction_size.toString())
        println("p2 fraction_size=" + posit_2.fraction_size.toString())
        println("fraction=" + fraction.toString())
        var comparare: Int = (1 << (2*size-1))
        println("comparare=" + comparare.toString())
        println("fraction_1=" + fraction_1.toString())
        println("fraction_2=" + fraction_2.toString())
        if(fraction == 0) {
            return 0
        }
        while(fraction < (1 << (2*size-2))) {
            fraction = fraction << 1
            exponent = exponent - 1
        }
        fraction = fraction - (1 << (2*size-2))
        println("fraction=" + fraction.toString())
        println("exponent=" + exponent.toString())
        println("regime=" + regime.toString())
        while(exponent < 0) {
            exponent = exponent + (1 << max_exponent_size)
            regime = regime - 1
        }
        println("exponent=" + exponent.toString())
        println("regime=" + regime.toString())
        while(exponent >= (1 << max_exponent_size)) {
            regime = regime + 1
            exponent = exponent - (1 << max_exponent_size)
        }
        println("regime=" + regime.toString())
        var sign: Int = posit_1.sign

        var return_value: Int = 0
        if(regime >= size-2) {
            return_value = ((1 << (size-1)) - 1)
            if((initial_sign^ sign) == 1) {
                return_value = ~(return_value - 1) & ((1 << (size)) - 1)
            }
            return return_value
        }
        if(regime <= -(size-2)) {
            return_value = 1
            if((initial_sign^ sign) == 1) {
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
        var aux: Int = 0
        var bitsMore: Int = 0

        println("regime=" + regime.toString())
        println("regime_size=" + regime_size.toString())
        println("fraction=" + fraction.toString())
        println("fraction_size=" + fraction_size.toString())
        println("exponent=" + exponent.toString())
        println("exponent_size=" + exponent_size.toString())
        if(max_exponent_size - exponent_size >= 2) {
            bitNplusOne = (exponent & (((1<<(max_exponent_size-exponent_size))-1)))
            println("before bitNplusOne=" + bitNplusOne.toString())
            bitNplusOne = (exponent & (((1<<(max_exponent_size-exponent_size))-1))) >>> (max_exponent_size-exponent_size-1)
            aux = (exponent & (((1<<(max_exponent_size-exponent_size-1))-1)))
            println("before aux=" + aux.toString())
            if(aux > 0 || fraction > 0) {
                bitsMore = 1
                fraction = 0
            }
            exponent = exponent >>> (max_exponent_size-exponent_size)
        } else {
            if(max_exponent_size - exponent_size == 1) {
                bitNplusOne = exponent & 1
                if(fraction > 0) {
                    bitsMore = 1
                    fraction = 0
                }
                exponent = exponent >> 1
            } else {
                println("before fraction=" + fraction.toString())
                bitNplusOne = (fraction & (((1<<(2*size-2-fraction_size))-1))) >>> (2*size-3-fraction_size)
                bitsMore = (fraction & ((1<<(2*size-3-fraction_size))-1))
                println("before bitsmore=" + bitsMore.toString())
                if((fraction & ((1<<(2*size-3-fraction_size))-1)) > 0 ) {
                    bitsMore = 1
                }
                fraction = fraction >>> (2*size-2-fraction_size)
            }
        }
        out_posit.sign = 0
        out_posit.special_number = 0
        out_posit.regime = regime
        out_posit.regime_size = regime_size
        out_posit.exponent = exponent
        out_posit.exponent_size = exponent_size
        out_posit.fraction = fraction
        out_posit.fraction_size = fraction_size
        return_value = PositTestDecodeEncode.encode(out_posit, size, max_exponent_size)
        println("fraction=" + fraction.toString())
        println("bitNplusOne=" + bitNplusOne.toString())
        println("bitsMore=" + bitsMore.toString())
        println("return_value=" + return_value.toString())
        if((bitNplusOne > 0) || (bitsMore > 0)) {
            return_value = return_value + 1
        }
        
        println("return_value=" + return_value.toString())
        if( (sign ^ initial_sign) == 1) {
            return_value = (~(return_value-1) & ((1 << (size-1)) - 1))
        }
        println("return_value=" + return_value.toString())
        return_value = ((sign ^ initial_sign) << (size-1)) | return_value
        println("return_value=" + return_value.toString())
        return return_value
    }
}


class TesterSubPosit(dut : PositSub) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    /**/
    var index: BigInt = new BigInteger("c0000000", 16)
    var index2: Int = Integer.parseInt("40000000", 16)
    var jindex: BigInt = new BigInteger("00000000", 16)
    var jindex2: Int = Integer.parseInt("01100000000000000000000000000000", 2)
    poke(dut.io.i_bits_1, index)
    poke(dut.io.i_bits_2, jindex)
    step(1)
    aux = PositTestSub.int_sub(index2, jindex2, 32, 3)
    println("index is: " + index.toString)
    println("jindex is: " + jindex.toString)
    expect(dut.io.o_bits, aux)
    println("Sign is: " + peek(dut.io.o_posit.sign).toString)
    println("Special number is: " + peek(dut.io.o_posit.special_number).toString)
    println("Regim is: " + peek(dut.io.o_posit.regime).toString)
    println("exponent is: " + peek(dut.io.o_posit.exponent).toString(2))
    println("fraction is: " + peek(dut.io.o_posit.fraction).toString(2))
    println("Regim size is: " + peek(dut.io.o_posit.regime_size).toString)
    println("Exponent size is: " + peek(dut.io.o_posit.exponent_size).toString)
    println("MAx exponent size is: " + peek(dut.io.o_posit.max_exponent_size).toString)
    println("fraction size is: " + peek(dut.io.o_posit.fraction_size).toString)
    println("debug_1 is: " + peek(dut.io.debug_1).toString)
    println("debug_2 is: " + peek(dut.io.debug_2).toString)
    /*
    
    for (index <- 128 until 256;
        jindex <- 128 until 256) {
        poke(dut.io.i_bits_1, index)
        poke(dut.io.i_bits_2, jindex)
        step(1)
        aux = PositTestSub.int_sub(index, jindex, 8, 1)
        println("index is: " + index.toString)
        println("jindex is: " + jindex.toString)
        expect(dut.io.o_bits, aux)
    }
    */
}

object TesterSubPosit extends App {
    chisel3.iotesters.Driver(() => new PositSub(3, 32)) { c => new TesterSubPosit(c) }
}