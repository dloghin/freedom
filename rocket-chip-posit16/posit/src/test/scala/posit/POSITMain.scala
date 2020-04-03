package posit

import chisel3._
import chisel3.iotesters._

class TestPosit (val es: Int, val size : Int) {
    var sign: Int = 0
    var special_number: Int = 0
    var regime: Int = 0
    var exponent: Int = 0
    var fraction: Int = 0
    var regime_size: Int = 0
    var exponent_size: Int = 0
    var fraction_size: Int = 0
    var max_exponent_size: Int = es
}

object PositTestDecodeEncode {
    def decode(a : Int, size: Int, max_exponent_size : Int) : TestPosit = {
        var my_posit: TestPosit = new TestPosit(max_exponent_size, size)

        if(a == 0) {
            my_posit.sign = 0
            my_posit.special_number = 1
            my_posit.regime = -(size-1)
            my_posit.regime_size = size-1
            my_posit.exponent = 0
            my_posit.exponent_size = 0
            my_posit.fraction = 0
            my_posit.fraction_size = 0
            return my_posit
        }
        if(a == (1 << (size-1))) {
            my_posit.sign = 1
            my_posit.special_number = 1
            my_posit.regime = -(size-1)
            my_posit.regime_size = size-1
            my_posit.exponent = 0
            my_posit.exponent_size = 0
            my_posit.fraction = 0
            my_posit.fraction_size = 0
            return my_posit
        }
        my_posit.sign = (a >>> (size-1)) & 1
        my_posit.special_number = 0
        var b: Int = 0
        if(my_posit.sign > 0) {
            b = (~a & ((1<<(size-1))-1)) + 1
        } else {
            b = a & ((1<<(size-1))-1)
        }
        if(b == ((1 << (size-1)) - 1) ) {
            my_posit.special_number = 0
            my_posit.regime = (size-2)
            my_posit.regime_size = size-1
            my_posit.exponent = 0
            my_posit.exponent_size = 0
            my_posit.fraction = 0
            my_posit.fraction_size = 0
            return my_posit
        }
        //println("B=" + b.toString())
        var valR: Int = (b >>> (size -2)) & 1
        //println("ValR=" + valR.toString())
        var countReg : Int = 1
        var sem: Int = 0
        var aux: Int = 0
        for (index <- 0 to (size-3)) {
            aux = (b >>> (size-3-index)) & 1
            //println("index=" + index.toString())
            //println("aux=" + aux.toString())
            if(aux == valR && sem == 0) {
                countReg = countReg + 1
            } else {
                sem = 1
            }
        }
        var regime: Int = 0
        var regime_size: Int = 0
        if(valR == 0) {
            regime = -countReg
            regime_size = countReg + 1
        } else {
            regime = countReg - 1
            regime_size = countReg + 1
        }
        //println("regime=" + regime.toString())
        //println("regime_size=" + regime_size.toString())
        var exponent: Int = 0
        var exponent_size: Int = 0
        exponent_size = size - 1 - regime_size
        if (exponent_size > max_exponent_size) {
            exponent_size = max_exponent_size
        }
        var fraction: Int = 0
        var fraction_size: Int = 0
        fraction_size = size - 1 - regime_size - max_exponent_size
        if (fraction_size <= 0) {
            fraction_size = 0;
            fraction = 0;
        } else {
            fraction = (b & ((1 << (fraction_size))-1))
        }
        //println("fraction=" + fraction.toString())
        //println("fraction_size=" + fraction_size.toString())
        if(exponent_size <= 0) {
            exponent_size = 0
            exponent = 0
        } else {
            exponent = (b & ((1 << (exponent_size + fraction_size))-1)) >> fraction_size
        }
        //println("exponent=" + exponent.toString())
        //println("exponent_size=" + exponent_size.toString())
        my_posit.regime = regime
        my_posit.regime_size = regime_size
        my_posit.exponent = exponent
        my_posit.exponent_size = exponent_size
        my_posit.fraction = fraction
        my_posit.fraction_size = fraction_size
        
        return my_posit
    }

    def decode(a : BigInt, size: Int, max_exponent_size : Int) : TestPosit = {
        var my_posit: TestPosit = new TestPosit(max_exponent_size, size)

        if(a == 0) {
            my_posit.sign = 0
            my_posit.special_number = 1
            my_posit.regime = -(size-1)
            my_posit.regime_size = size-1
            my_posit.exponent = 0
            my_posit.exponent_size = 0
            my_posit.fraction = 0
            my_posit.fraction_size = 0
            return my_posit
        }
        if(a == (1 << (size-1))) {
            my_posit.sign = 1
            my_posit.special_number = 1
            my_posit.regime = -(size-1)
            my_posit.regime_size = size-1
            my_posit.exponent = 0
            my_posit.exponent_size = 0
            my_posit.fraction = 0
            my_posit.fraction_size = 0
            return my_posit
        }
        my_posit.sign = ((a >> (size-1)) & 1).intValue
        my_posit.special_number = 0
        var b: BigInt = 0
        if(my_posit.sign > 0) {
            b = (~a & ((1<<(size-1))-1)) + 1
        } else {
            b = a & ((1<<(size-1))-1)
        }
        if(b == ((1 << (size-1)) - 1) ) {
            my_posit.special_number = 0
            my_posit.regime = (size-2)
            my_posit.regime_size = size-1
            my_posit.exponent = 0
            my_posit.exponent_size = 0
            my_posit.fraction = 0
            my_posit.fraction_size = 0
            return my_posit
        }
        //println("B=" + b.toString())
        var valR: BigInt = (b >> (size -2)) & 1
        //var valR: Int = valR2.intValue
        //println("ValR=" + valR.toString())
        var countReg : Int = 1
        var sem: Int = 0
        var aux: BigInt = 0
        for (index <- 0 to (size-3)) {
            aux = (b >> (size-3-index)) & 1
            //println("index=" + index.toString())
            //println("aux=" + aux.toString())
            if(aux == valR && sem == 0) {
                countReg = countReg + 1
            } else {
                sem = 1
            }
        }
        var regime: Int = 0
        var regime_size: Int = 0
        if(valR == 0) {
            regime = -countReg
            regime_size = countReg + 1
        } else {
            regime = countReg - 1
            regime_size = countReg + 1
        }
        //println("regime=" + regime.toString())
        //println("regime_size=" + regime_size.toString())
        var exponent: Int = 0
        var exponent_size: Int = 0
        exponent_size = size - 1 - regime_size
        if (exponent_size > max_exponent_size) {
            exponent_size = max_exponent_size
        }
        var fraction: Int = 0
        var fraction_size: Int = 0
        fraction_size = size - 1 - regime_size - max_exponent_size
        if (fraction_size <= 0) {
            fraction_size = 0;
            fraction = 0;
        } else {
            fraction = (b & ((1 << (fraction_size))-1)).intValue
        }
        //println("fraction=" + fraction.toString())
        //println("fraction_size=" + fraction_size.toString())
        if(exponent_size <= 0) {
            exponent_size = 0
            exponent = 0
        } else {
            exponent = ((b & ((1 << (exponent_size + fraction_size))-1)) >> fraction_size).intValue
        }
        //println("exponent=" + exponent.toString())
        //println("exponent_size=" + exponent_size.toString())
        my_posit.regime = regime
        my_posit.regime_size = regime_size
        my_posit.exponent = exponent
        my_posit.exponent_size = exponent_size
        my_posit.fraction = fraction
        my_posit.fraction_size = fraction_size
        
        return my_posit
    }

    def encode(a : TestPosit, size: Int, max_exponent_size : Int) : Int = {
        var return_value: Int = 0

        if(a.special_number == 1) {
            if(a.sign == 1) {
                return_value = 1 << (size-1)
            } else {
                return_value = 0
            }
            return return_value
        }

        if(a.regime_size == size-1) {
            if(a.regime == size-2) {
                return_value = ((1 << (size-1)) - 1)
                if(a.sign == 1) {
                    return_value = ~(return_value - 1) & ((1 << (size)) - 1)
                }
                return return_value
            }
        }
        var regime: Int = 0
        regime = ((1 << (a.regime_size-1)) - 1) << 1
        if (a.regime < 0) {
            regime = 1
        }
        var value: Int = 0
        value = (regime << (a.exponent_size + a.fraction_size)) | (a.exponent << a.fraction_size) | a.fraction
        println("a.exponent_size=" + a.exponent_size.toString())
        println("a.fraction_size=" + a.fraction_size.toString())
        println("value=" + value.toString())
        if(a.sign == 1) {
            value = (~(value-1) & ((1 << (size-1)) - 1))
        }
        return_value = (a.sign << (size-1)) | value
        return return_value
    }

}
class TesterSimplePosit(dut : DecodePosit) extends PeekPokeTester(dut) {
    //poke(dut.io.i_bits, "b0_1110_101_1110_1010_0000_0000_0000_0001".asUInt(32.W))
    //poke(dut.io.i_bits, "b1_0000_000_0000_0000_0000_0000_0000_0001".asUInt(32.W))
    poke(dut.io.i_bits, "b0_1111_110".asUInt(8.W))
    step(1)
    println("Sign is: " + peek(dut.io.o_posit.sign).toString)
    println("Special number is: " + peek(dut.io.o_posit.special_number).toString)
    println("Regim is: " + peek(dut.io.o_posit.regime).toString)
    println("exponent is: " + peek(dut.io.o_posit.exponent).toString(2))
    println("fraction is: " + peek(dut.io.o_posit.fraction).toString(2))
    println("Regim size is: " + peek(dut.io.o_posit.regime_size).toString)
    println("Exponent size is: " + peek(dut.io.o_posit.exponent_size).toString)
    println("MAx exponent size is: " + peek(dut.io.o_posit.max_exponent_size).toString)
    println("fraction size is: " + peek(dut.io.o_posit.fraction_size).toString)
}

object TesterSimplePosit extends App {
    chisel3.iotesters.Driver(() => new DecodePosit(2, 8)) { c => new TesterSimplePosit(c) }
}

class TesterDeEnPosit(dut : DeEnPosit) extends PeekPokeTester(dut) {
    /*poke(dut.io.i_bits, 126)
    //poke(dut.io.i_bits, "b0_0000_000_0000_0000_0000_0000_0000_0000".asUInt(32.W))
    step(1)
    println("Sign is: " + peek(dut.io.o_posit.sign).toString)
    println("Special number is: " + peek(dut.io.o_posit.special_number).toString)
    println("Regim is: " + peek(dut.io.o_posit.regime).toString)
    println("exponent is: " + peek(dut.io.o_posit.exponent).toString(2))
    println("fraction is: " + peek(dut.io.o_posit.fraction).toString(2))
    println("Regim size is: " + peek(dut.io.o_posit.regime_size).toString)
    println("Exponent size is: " + peek(dut.io.o_posit.exponent_size).toString)
    println("MAx exponent size is: " + peek(dut.io.o_posit.max_exponent_size).toString)
    println("fraction size is: " + peek(dut.io.o_posit.fraction_size).toString)o.o_bits).toString(2))
    */
    
    for (value <- 0 until  65535) {
        poke(dut.io.i_bits, value)
        step(1)
        expect(dut.io.o_bits, value)
    }
    


}

object TesterDeEnPosit extends App {
    chisel3.iotesters.Driver(() => new DeEnPosit(3, 16)) { c => new TesterDeEnPosit(c) }
}

class TesterDePosit(dut : DecodePosit) extends PeekPokeTester(dut) {
    var my_posit: TestPosit = new TestPosit(2, 8)
    /*
    var value: Int = 8 
    poke(dut.io.i_bits, value)
    my_posit = PositTestDecodeEncode.decode(value, 8, 2)
    step(1)
    expect(dut.io.o_posit.sign, my_posit.sign)
    expect(dut.io.o_posit.special_number, my_posit.special_number)
    expect(dut.io.o_posit.regime, my_posit.regime)
    expect(dut.io.o_posit.exponent, my_posit.exponent)
    expect(dut.io.o_posit.fraction, my_posit.fraction)
    expect(dut.io.o_posit.regime_size, my_posit.regime_size)
    expect(dut.io.o_posit.exponent_size, my_posit.exponent_size)
    expect(dut.io.o_posit.fraction_size, my_posit.fraction_size)
    */
    for (value <- 0 until  255) {
        my_posit = PositTestDecodeEncode.decode(value, 8, 2)
        expect(dut.io.o_posit.sign, my_posit.sign)
        expect(dut.io.o_posit.special_number, my_posit.special_number)
        expect(dut.io.o_posit.regime, my_posit.regime)
        expect(dut.io.o_posit.exponent, my_posit.exponent)
        expect(dut.io.o_posit.fraction, my_posit.fraction)
        expect(dut.io.o_posit.regime_size, my_posit.regime_size)
        expect(dut.io.o_posit.exponent_size, my_posit.exponent_size)
        expect(dut.io.o_posit.fraction_size, my_posit.fraction_size)
        step(1)
        poke(dut.io.i_bits, value)
    }
    //*/
    


}

object TesterDePosit extends App {
    chisel3.iotesters.Driver(() => new DecodePosit(2, 8)) { c => new TesterDePosit(c) }
}

class TesterEnPosit(dut : EncodePosit) extends PeekPokeTester(dut) {
    var my_posit: TestPosit = new TestPosit(2, 8)
    var aux: Int = 0;
    
    for (value <- 0 until  256) {
        my_posit = PositTestDecodeEncode.decode(value, 8, 2)
        poke(dut.io.i_posit.sign, my_posit.sign)
        poke(dut.io.i_posit.special_number, my_posit.special_number)
        poke(dut.io.i_posit.regime, my_posit.regime)
        poke(dut.io.i_posit.exponent, my_posit.exponent)
        poke(dut.io.i_posit.fraction, my_posit.fraction)
        poke(dut.io.i_posit.regime_size, my_posit.regime_size)
        poke(dut.io.i_posit.exponent_size, my_posit.exponent_size)
        poke(dut.io.i_posit.fraction_size, my_posit.fraction_size)
        poke(dut.io.i_posit.max_exponent_size, my_posit.max_exponent_size)
        step(1)
        aux = PositTestDecodeEncode.encode(my_posit, 8, 2)
        expect(dut.io.o_bits, aux)
        expect(dut.io.o_bits, value)
    }
    //*/
    


}

object TesterEnPosit extends App {
    chisel3.iotesters.Driver(() => new EncodePosit(2, 8)) { c => new TesterEnPosit(c) }
}



