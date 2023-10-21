// See LICENSE for license details.
package sifive.freedom.everywhere.e300artydevkitddr3

import Chisel._
import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.system._
import freechips.rocketchip.tile._

import sifive.blocks.devices.mockaon._
import sifive.blocks.devices.gpio._
import sifive.blocks.devices.pwm._
import sifive.blocks.devices.spi._
import sifive.blocks.devices.uart._
import sifive.blocks.devices.i2c._

import sifive.fpgashells.devices.xilinx.xilinxartya7mig.{MemoryXilinxDDRKey,XilinxArtyA7MIGParams}

// Default FreedomEConfig
/*
class DefaultFreedomEConfig extends Config (
  new WithNMemoryChannels(1)     ++
  new WithExtMemSize(0x10000000) ++
  new WithNBreakpoints(2)        ++
  new WithNExtTopInterrupts(0)   ++
  new WithJtagDTM                ++
  new WithL1ICacheWays(2)        ++
  new WithL1ICacheSets(128)      ++
  new WithDefaultBtb             ++
  new TinyFPUConfig
)
*/

class DefaultFreedomEConfig extends Config (
  new WithJtagDTM            ++
  new WithNMemoryChannels(1) ++
  new WithNSmallCores(1)     ++
  new BaseConfig
)

// Freedom E300 Arty Dev Kit Peripherals
class E300DevKitDDR3Peripherals extends Config((site, here, up) => {
  case PeripheryGPIOKey => List(
    GPIOParams(address = 0x10012000, width = 32, includeIOF = true))
  case PeripheryPWMKey => List(
    PWMParams(address = 0x10015000, cmpWidth = 8),
    PWMParams(address = 0x10025000, cmpWidth = 16),
    PWMParams(address = 0x10035000, cmpWidth = 16))
  case PeripherySPIKey => List(
    SPIParams(csWidth = 4, rAddress = 0x10024000, defaultSampleDel = 3),
    SPIParams(csWidth = 1, rAddress = 0x10034000, defaultSampleDel = 3))
  case PeripherySPIFlashKey => List(
    SPIFlashParams(
      fAddress = 0x20000000,
      rAddress = 0x10014000,
      defaultSampleDel = 3))
  case PeripheryUARTKey => List(
    UARTParams(address = 0x10013000),
    UARTParams(address = 0x10023000))
  case PeripheryI2CKey => List(
    I2CParams(address = 0x10016000))
  case PeripheryMockAONKey =>
    MockAONParams(address = 0x10000000)
  case PeripheryMaskROMKey => List(
    MaskROMParams(address = 0x10000, name = "BootROM"))
})

// Freedom E300 Arty Dev Kit Peripherals
class E300ArtyDevKitDDR3Config extends Config(
  new WithNExtTopInterrupts(0)   ++
  new E300DevKitDDR3Peripherals    ++
  new DefaultFreedomEConfig().alter((site,here,up) => {
    case DTSTimebase => BigInt(32768)
    case PeripheryBusKey => up(PeripheryBusKey, site).copy(frequency = 50000000) // 50 MHz periphery
    case MemoryXilinxDDRKey => XilinxArtyA7MIGParams(address = Seq(AddressSet(0x80000000L,0x10000000L-1))) // 256GB
    case JtagDTMKey => new JtagDTMConfig (
      idcodeVersion = 2,
      idcodePartNum = 0x000,
      idcodeManufId = 0x489,
      debugIdleCycles = 5)
  })
)
