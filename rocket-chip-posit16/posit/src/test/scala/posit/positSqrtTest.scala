package posit

import chisel3._
import chisel3.iotesters._

import java.math.BigInteger
import scala.math.sqrt

object PositTestSqrt {
    def int_sqrt(a : BigInt, size: Int, max_exponent_size : Int) : Int = {
        var posit_1: TestPosit = new TestPosit(max_exponent_size, size)
        var out_posit: TestPosit = new TestPosit(max_exponent_size, size)
        posit_1 = PositTestDecodeEncode.decode(a, size, max_exponent_size)

        if(posit_1.special_number==1) {
            return PositTestDecodeEncode.encode(posit_1, size, max_exponent_size)
        }

        var regime: Int = posit_1.regime >> 1
        var par_exponent: Int = posit_1.exponent + (1 << max_exponent_size) * (posit_1.regime & 1)
        var exponent: Int = par_exponent >> 1
        var fraction_1: Int = (1 << (size-2)) + (posit_1.fraction << (size-2-posit_1.fraction_size))
        /*
        var fraction_2: Int = fraction_1 << (par_exponent & 1)
        var fraction_3: Int = (sqrt(fraction_2)).toInt << ((size-2)/2)
        */
        var fraction_2: Int = fraction_1
        var fraction_3: Int = (sqrt(fraction_2)*sqrt(1+(par_exponent & 1))).toInt << ((size-2)/2)
        var fraction = fraction_3 - (1 << (size-2))
        println("p1 regime=" + posit_1.regime.toString())
        println("p1 exponent=" + posit_1.exponent.toString())
        println("p1 fraction=" + posit_1.fraction.toString())
        println("p1 fraction_size=" + posit_1.fraction_size.toString())
        println("fraction1=" + fraction_1.toString())
        println("fraction2=" + fraction_2.toString())
        println("fraction3=" + fraction_3.toString())
        println("fraction=" + fraction.toString())
        var sign: Int = posit_1.sign

        var return_value: Int = 0
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
                bitNplusOne = (fraction & (((1<<(size-2-fraction_size))-1))) >>> (size-3-fraction_size)
                bitsMore = (fraction & ((1<<(size-3-fraction_size))-1))
                println("before bitsmore=" + bitsMore.toString())
                if((fraction & ((1<<(size-3-fraction_size))-1)) > 0 ) {
                    bitsMore = 1
                }
                fraction = fraction >>> (size-2-fraction_size)
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
        
        if(sign == 1) {
            return_value = (~(return_value-1) & ((1 << (size-1)) - 1))
        }
        return_value = (sign << (size-1)) | return_value
        return return_value
    }
}

class TesterSqrtPosit(dut : PositSqrt) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    /*
    var index: BigInt = new BigInteger("00000003", 16)
    var index2: Int = Integer.parseInt("40000000", 16)
    poke(dut.io.i_bits, index)
    poke(dut.io.i_ready, 1)
    for (jindex <- 0 until 17) {
        step(1)
        poke(dut.io.i_ready, 0)
    }
    aux = PositTestSqrt.int_sqrt(index, 32, 3)
    println("index is: " + index.toString)
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
    //println("Sign is: " + peek(dut.io.o_posit.sign).toString)
    */
    /**/
    for (index <- 0 until 65535) {
        poke(dut.io.i_bits, index)
        poke(dut.io.i_ready, 1)
        for (jindex <- 0 until 17) {
            step(1)
            poke(dut.io.i_ready, 0)
        }
        aux = PositTestSqrt.int_sqrt(index, 32, 3)
        println("index is: " + index.toString)
        expect(dut.io.o_bits, aux)
    }
    /**/
}

object TesterSqrtPosit extends App {
    chisel3.iotesters.Driver(() => new PositSqrt(3, 32)) { c => new TesterSqrtPosit(c) }
}
//PositSqrt(3, 32)


class TesterSqrtUint(dut : UIntSqrt) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    /**/
    var index: BigInt = new BigInteger("c0000000", 16)
    var index2: Int = Integer.parseInt("0000008c", 16)
    /*
    aux = sqrt(index2).toInt

    poke(dut.io.i_bits, index2)
    poke(dut.io.i_ready, 1)
    println("value is: " + index2.toString)
    println("Index is: " + index.toString)
    println("o_ready is: " + peek(dut.io.o_ready).toString)
    println("o_bits number is: " + peek(dut.io.o_bits).toString)
    println("o_R number is: " + peek(dut.io.o_R).toString)
    println("o_D number is: " + peek(dut.io.o_D).toString)
    println("o_counter number is: " + peek(dut.io.o_counter).toString)
    println("o_wR1 number is: " + peek(dut.io.o_wR1).toString)
    println("o_wR2_1 number is: " + peek(dut.io.o_wR2_1).toString)
    println("o_wR2_2 number is: " + peek(dut.io.o_wR2_2).toString)
    for (index <- 0 until 5) {
        step(1)
        poke(dut.io.i_ready, 0)
        println("Index is: " + index.toString)
        println("o_ready is: " + peek(dut.io.o_ready).toString)
        println("o_bits/o_Q number is: " + peek(dut.io.o_bits).toString)
        println("o_R number is: " + peek(dut.io.o_R).toString)
        println("o_D number is: " + peek(dut.io.o_D).toString)
        println("o_counter number is: " + peek(dut.io.o_counter).toString)
        println("o_wR1 number is: " + peek(dut.io.o_wR1).toString)
        println("o_wR2_1 number is: " + peek(dut.io.o_wR2_1).toString)
        println("o_wR2_2 number is: " + peek(dut.io.o_wR2_2).toString)
    }
    expect(dut.io.o_bits, aux)
    expect(dut.io.o_ready, 1)
    */
    for (jindex <- 0 until 2147483647) {
        //println("jindex is: " + jindex.toString)
        poke(dut.io.i_bits, jindex)
        poke(dut.io.i_ready, 1)
        aux = sqrt(jindex).toInt
        for (index <- 0 until 17) {
            step(1)
            poke(dut.io.i_ready, 0)
        }
        expect(dut.io.o_bits, aux)
        expect(dut.io.o_ready, 1)
    }
    /*
    
    for (jndex <- 0 until 256;
        kindex <- 0 until 256) {
        poke(dut.io.i_bits_1, index)
        poke(dut.io.i_bits_2, jindex)
        step(1)
        aux = PositTestMul.int_multiply(index, jindex, 8, 2)
        println("index is: " + index.toString)
        println("jindex is: " + jindex.toString)
        expect(dut.io.o_bits, aux)
    }
    */
}

object TesterSqrtUint extends App {
    chisel3.iotesters.Driver(() => new UIntSqrt(16)) { c => new TesterSqrtUint(c) }
}
