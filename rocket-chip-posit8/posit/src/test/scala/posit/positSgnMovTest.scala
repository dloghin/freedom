package posit

import chisel3._
import chisel3.iotesters._

object PositTestSgnMov {
    def int_SGNJ(a : Int, b : Int, size: Int, max_exponent_size : Int) : Int = {
        var out_posit: TestPosit = new TestPosit(max_exponent_size, size)
        var posit_1: TestPosit = new TestPosit(max_exponent_size, size)
        var posit_2: TestPosit = new TestPosit(max_exponent_size, size)
        var return_value: Int = 0
        posit_1 = PositTestDecodeEncode.decode(a, size, max_exponent_size)
        posit_2 = PositTestDecodeEncode.decode(b, size, max_exponent_size)

        out_posit.sign = posit_2.sign
        out_posit.special_number = posit_1.special_number
        out_posit.regime = posit_1.regime
        out_posit.regime_size = posit_1.regime_size
        out_posit.exponent = posit_1.exponent
        out_posit.exponent_size = posit_1.exponent_size
        out_posit.fraction = posit_1.fraction
        out_posit.fraction_size = posit_1.fraction_size
        return_value = PositTestDecodeEncode.encode(out_posit, size, max_exponent_size)
        return return_value
    }

    def int_SGNJN(a : Int, b : Int, size: Int, max_exponent_size : Int) : Int = {
        var out_posit: TestPosit = new TestPosit(max_exponent_size, size)
        var posit_1: TestPosit = new TestPosit(max_exponent_size, size)
        var posit_2: TestPosit = new TestPosit(max_exponent_size, size)
        var return_value: Int = 0
        posit_1 = PositTestDecodeEncode.decode(a, size, max_exponent_size)
        posit_2 = PositTestDecodeEncode.decode(b, size, max_exponent_size)

        out_posit.sign = 1 - posit_2.sign
        out_posit.special_number = posit_1.special_number
        out_posit.regime = posit_1.regime
        out_posit.regime_size = posit_1.regime_size
        out_posit.exponent = posit_1.exponent
        out_posit.exponent_size = posit_1.exponent_size
        out_posit.fraction = posit_1.fraction
        out_posit.fraction_size = posit_1.fraction_size
        return_value = PositTestDecodeEncode.encode(out_posit, size, max_exponent_size)
        return return_value
    }

    def int_SGNJX(a : Int, b : Int, size: Int, max_exponent_size : Int) : Int = {
        var out_posit: TestPosit = new TestPosit(max_exponent_size, size)
        var posit_1: TestPosit = new TestPosit(max_exponent_size, size)
        var posit_2: TestPosit = new TestPosit(max_exponent_size, size)
        var return_value: Int = 0
        posit_1 = PositTestDecodeEncode.decode(a, size, max_exponent_size)
        posit_2 = PositTestDecodeEncode.decode(b, size, max_exponent_size)

        out_posit.sign = posit_2.sign ^ posit_1.sign
        out_posit.special_number = posit_1.special_number
        out_posit.regime = posit_1.regime
        out_posit.regime_size = posit_1.regime_size
        out_posit.exponent = posit_1.exponent
        out_posit.exponent_size = posit_1.exponent_size
        out_posit.fraction = posit_1.fraction
        out_posit.fraction_size = posit_1.fraction_size
        return_value = PositTestDecodeEncode.encode(out_posit, size, max_exponent_size)
        return return_value
    }

}


class TesterSGNJPosit(dut : PositSGNJ) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    for (index <- 0 until 256;
        jindex <- 0 until 256) {
        poke(dut.io.i_bits_1, index)
        poke(dut.io.i_bits_2, jindex)
        step(1)
        aux = PositTestSgnMov.int_SGNJ(index, jindex, 8, 1)
        println("index is: " + index.toString)
        println("jindex is: " + jindex.toString)
        expect(dut.io.o_bits, aux)
    }
    /**/
}

object TesterSGNJPosit extends App {
    chisel3.iotesters.Driver(() => new PositSGNJ(1, 8)) { c => new TesterSGNJPosit(c) }
}

class TesterSGNJNPosit(dut : PositSGNJN) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    for (index <- 0 until 256;
        jindex <- 0 until 256) {
        poke(dut.io.i_bits_1, index)
        poke(dut.io.i_bits_2, jindex)
        step(1)
        aux = PositTestSgnMov.int_SGNJN(index, jindex, 8, 1)
        println("index is: " + index.toString)
        println("jindex is: " + jindex.toString)
        expect(dut.io.o_bits, aux)
    }
    /**/
}

object TesterSGNJNPosit extends App {
    chisel3.iotesters.Driver(() => new PositSGNJN(1, 8)) { c => new TesterSGNJNPosit(c) }
}

class TesterSGNJXPosit(dut : PositSGNJX) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    for (index <- 0 until 256;
        jindex <- 0 until 256) {
        poke(dut.io.i_bits_1, index)
        poke(dut.io.i_bits_2, jindex)
        step(1)
        aux = PositTestSgnMov.int_SGNJX(index, jindex, 8, 1)
        println("index is: " + index.toString)
        println("jindex is: " + jindex.toString)
        expect(dut.io.o_bits, aux)
    }
    /**/
}

object TesterSGNJXPosit extends App {
    chisel3.iotesters.Driver(() => new PositSGNJX(1, 8)) { c => new TesterSGNJXPosit(c) }
}

