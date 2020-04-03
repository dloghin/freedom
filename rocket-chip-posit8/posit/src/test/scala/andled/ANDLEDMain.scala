package andled

import chisel3._
import chisel3.iotesters._

class TesterSimple(dut : ANDLED) extends PeekPokeTester(dut) {
    poke(dut.io.value1, 0)
    poke(dut.io.value2, 1)
    poke(dut.io.sel, 0)
    step(1)
    println("Result is: " + peek(dut.io.output).toString)
    expect(dut.io.output, 1)
}

object TesterSimple extends App {
    chisel3.iotesters.Driver(() => new ANDLED()) { c => new TesterSimple(c) }
}

class TesterSimplePosit(dut : DecodePosit32) extends PeekPokeTester(dut) {
    poke(dut.io.i_bits, "b0_1110_111_1110_1010_0000_0000_0000_0001".asUInt(32.W))
    step(1)
    println("Sign is: " + peek(dut.io.o_posit.sign).toString)
    println("Regim is: " + peek(dut.io.o_posit.regime).toString)
    println("exponent is: " + peek(dut.io.o_posit.exponent).toString(2))
    println("fraction is: " + peek(dut.io.o_posit.fraction).toString(2))
}

object TesterSimplePosit extends App {
    chisel3.iotesters.Driver(() => new DecodePosit32()) { c => new TesterSimplePosit(c) }
}