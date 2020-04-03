package posit

import chisel3._
import chisel3.iotesters._
import java.math.BigInteger

object PositTestPositFloat {
    def int_SGNJ(a : BigInt, size: Int, max_exponent_size : Int) : Double = {
        var posit_1: TestPosit = new TestPosit(max_exponent_size, size)
        var return_value: Double = 0
        posit_1 = PositTestDecodeEncode.decode(a, size, max_exponent_size)

        println("p1 regime=" + posit_1.regime.toString())
        println("p1 exponent=" + posit_1.exponent.toString())
        println("p1 fraction=" + posit_1.fraction.toString())
        println("p1 fraction_size=" + posit_1.fraction_size.toString())
        println("p1 sign=" + posit_1.sign.toString())
        //return_value = (2^(2^max_exponent_size))^posit_1.regime

        return_value =  scala.math.pow(scala.math.pow(2,scala.math.pow(2,max_exponent_size).toInt).toInt, posit_1.regime).toDouble
        println("return_value is: " + return_value.toString)
        return_value =  return_value * scala.math.pow(2,posit_1.exponent).toDouble

        //println("return_value=" + return_value.toString())
        //return_value = return_value * (2^posit_1.exponent)
        println("return_value=" + return_value.toString())
        return_value = return_value * (1 + (posit_1.fraction.toDouble / (scala.math.pow(2,posit_1.fraction_size).toDouble)))
        println("return_value=" + return_value.toString())
        if(posit_1.sign == 1) {
            return_value = return_value * -1
        } else {
            return_value = return_value
        }
        println("return_value=" + return_value.toString())
        return return_value
    }

}

class TesterPositFloat(dut : PositSGNJX) extends PeekPokeTester(dut) {
    var aux: Double = 0;

    var jindex: BigInt = new BigInteger("46487eca", 16)
    aux = PositTestPositFloat.int_SGNJ(jindex, 32, 3)
    println("jindex is: " + jindex.toString)
    println("aux is: " + aux.toString)
}

object TesterPositFloat extends App {
    chisel3.iotesters.Driver(() => new PositSGNJX(1, 8)) { c => new TesterPositFloat(c) }
}

