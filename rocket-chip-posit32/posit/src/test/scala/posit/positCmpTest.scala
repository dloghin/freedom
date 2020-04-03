package posit

import chisel3._
import chisel3.iotesters._

object PositTestCMP {
    def int_LE(a : Int, b : Int, size: Int, max_exponent_size : Int) : Int = {
        var out_posit: TestPosit = new TestPosit(max_exponent_size, size)
        if( ((a>>(size-1))^(b>>(size-1)))==0) {
            if((a>>(size-1)) == 0) {
                if(a <= b) {
                    return 1
                } else {
                    return 0
                }
            } else {
                if(a < b) {
                    return 0
                } else {
                    return 1
                }
            }
        } else {
             if((a>>(size-1)) == 0) {
                 return 0
             } else {
                 return 1
             }
        }
    }

    def int_L(a : Int, b : Int, size: Int, max_exponent_size : Int) : Int = {
        var out_posit: TestPosit = new TestPosit(max_exponent_size, size)
        if( ((a>>(size-1))^(b>>(size-1)))==0) {
            if((a>>(size-1)) == 0) {
                if(a < b) {
                    return 1
                } else {
                    return 0
                }
            } else {
                if(a <= b) {
                    return 0
                } else {
                    return 1
                }
            }
        } else {
             if((a>>(size-1)) == 0) {
                 return 0
             } else {
                 return 1
             }
        }
    }


    def int_E(a : Int, b : Int, size: Int, max_exponent_size : Int) : Int = {
        var out_posit: TestPosit = new TestPosit(max_exponent_size, size)
        if(a == b) {
            return 1
        } else {
            return 0
        }
    }
}


class TesterLEPosit(dut : PositLE) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    for (index <- 0 until 256;
        jindex <- 0 until 256) {
        poke(dut.io.i_bits_1, index)
        poke(dut.io.i_bits_2, jindex)
        step(1)
        aux = PositTestCMP.int_LE(index, jindex, 8, 1)
        println("index is: " + index.toString)
        println("jindex is: " + jindex.toString)
        expect(dut.io.o_result, aux)
    }
    /**/
}

object TesterLEPosit extends App {
    chisel3.iotesters.Driver(() => new PositLE(1, 8)) { c => new TesterLEPosit(c) }
}


class TesterLPosit(dut : PositL) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    for (index <- 0 until 256;
        jindex <- 0 until 256) {
        poke(dut.io.i_bits_1, index)
        poke(dut.io.i_bits_2, jindex)
        step(1)
        aux = PositTestCMP.int_L(index, jindex, 8, 1)
        println("index is: " + index.toString)
        println("jindex is: " + jindex.toString)
        expect(dut.io.o_result, aux)
    }
    /**/
}

object TesterLPosit extends App {
    chisel3.iotesters.Driver(() => new PositL(1, 8)) { c => new TesterLPosit(c) }
}


class TesterEPosit(dut : PositE) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    for (index <- 0 until 256;
        jindex <- 0 until 256) {
        poke(dut.io.i_bits_1, index)
        poke(dut.io.i_bits_2, jindex)
        step(1)
        aux = PositTestCMP.int_E(index, jindex, 8, 1)
        println("index is: " + index.toString)
        println("jindex is: " + jindex.toString)
        expect(dut.io.o_result, aux)
    }
    /**/
}

object TesterEPosit extends App {
    chisel3.iotesters.Driver(() => new PositE(1, 8)) { c => new TesterEPosit(c) }
}