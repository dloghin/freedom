package posit

import chisel3._
import chisel3.iotesters._

object PositTestMinMax {
    def int_Min(a : Int, b : Int, size: Int, max_exponent_size : Int) : Int = {
        var out_posit: TestPosit = new TestPosit(max_exponent_size, size)
        if( ((a>>(size-1))^(b>>(size-1)))==0) {
            if((a>>(size-1)) == 0) {
                if(a <= b) {
                    return a
                } else {
                    return b
                }
            } else {
                if(a < b) {
                    return b
                } else {
                    return a
                }
            }
        } else {
             if((a>>(size-1)) == 0) {
                 return b
             } else {
                 return a
             }
        }
    }

    def int_Max(a : Int, b : Int, size: Int, max_exponent_size : Int) : Int = {
        var out_posit: TestPosit = new TestPosit(max_exponent_size, size)
        if( ((a>>(size-1))^(b>>(size-1)))==0) {
            if((a>>(size-1)) == 0) {
                if(a < b) {
                    return b
                } else {
                    return a
                }
            } else {
                if(a <= b) {
                    return a
                } else {
                    return b
                }
            }
        } else {
             if((a>>(size-1)) == 0) {
                 return a
             } else {
                 return b
             }
        }
    }

}


class TesterMinPosit(dut : PositMin) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    for (index <- 0 until 256;
        jindex <- 0 until 256) {
        poke(dut.io.i_bits_1, index)
        poke(dut.io.i_bits_2, jindex)
        step(1)
        aux = PositTestMinMax.int_Min(index, jindex, 8, 1)
        println("index is: " + index.toString)
        println("jindex is: " + jindex.toString)
        expect(dut.io.o_bits, aux)
    }
    /**/
}

object TesterMinPosit extends App {
    chisel3.iotesters.Driver(() => new PositMin(1, 8)) { c => new TesterMinPosit(c) }
}


class TesterMaxPosit(dut : PositMax) extends PeekPokeTester(dut) {
    var aux: Int = 0;

    for (index <- 0 until 256;
        jindex <- 0 until 256) {
        poke(dut.io.i_bits_1, index)
        poke(dut.io.i_bits_2, jindex)
        step(1)
        aux = PositTestMinMax.int_Max(index, jindex, 8, 1)
        println("index is: " + index.toString)
        println("jindex is: " + jindex.toString)
        expect(dut.io.o_bits, aux)
    }
    /**/
}

object TesterMaxPosit extends App {
    chisel3.iotesters.Driver(() => new PositMax(1, 8)) { c => new TesterMaxPosit(c) }
}
