;buildInfoPackage: chisel3, version: 3.1.7, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-03-20 22:15:13.399, builtAtMillis: 1553120113399
circuit GCD : 
  module GCD : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip value1 : UInt<16>, flip value2 : UInt<16>, flip loadingValues : UInt<1>, outputGCD : UInt<16>, outputValid : UInt<1>}
    
    reg x : UInt, clock @[GCD.scala 22:15]
    reg y : UInt, clock @[GCD.scala 23:15]
    node _T_17 = gt(x, y) @[GCD.scala 25:10]
    when _T_17 : @[GCD.scala 25:15]
      node _T_18 = sub(x, y) @[GCD.scala 25:24]
      node _T_19 = asUInt(_T_18) @[GCD.scala 25:24]
      node _T_20 = tail(_T_19, 1) @[GCD.scala 25:24]
      x <= _T_20 @[GCD.scala 25:19]
      skip @[GCD.scala 25:15]
    else : @[GCD.scala 26:16]
      node _T_21 = sub(y, x) @[GCD.scala 26:25]
      node _T_22 = asUInt(_T_21) @[GCD.scala 26:25]
      node _T_23 = tail(_T_22, 1) @[GCD.scala 26:25]
      y <= _T_23 @[GCD.scala 26:20]
      skip @[GCD.scala 26:16]
    when io.loadingValues : @[GCD.scala 28:26]
      x <= io.value1 @[GCD.scala 29:7]
      y <= io.value2 @[GCD.scala 30:7]
      skip @[GCD.scala 28:26]
    io.outputGCD <= x @[GCD.scala 33:16]
    node _T_25 = eq(y, UInt<1>("h00")) @[GCD.scala 34:23]
    io.outputValid <= _T_25 @[GCD.scala 34:18]
    
