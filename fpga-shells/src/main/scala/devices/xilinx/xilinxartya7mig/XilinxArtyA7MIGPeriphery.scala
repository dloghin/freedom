// See LICENSE for license details.
package sifive.fpgashells.devices.xilinx.xilinxartya7mig

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, AddressRange}

case object MemoryXilinxDDRKey extends Field[XilinxArtyA7MIGParams]

trait HasMemoryXilinxArtyA7MIG { this: BaseSubsystem =>
  val module: HasMemoryXilinxArtyA7MIGModuleImp

  val xilinxartya7mig = LazyModule(new XilinxArtyA7MIG(p(MemoryXilinxDDRKey)))

  xilinxartya7mig.node := mbus.toDRAMController(Some("xilinxArtyA7mig"))()
}

trait HasMemoryXilinxArtyA7MIGBundle {
  val xilinxartya7mig: XilinxArtyA7MIGIO
  def connectXilinxArtyA7MIGToPads(pads: XilinxArtyA7MIGPads) {
    pads <> xilinxartya7mig
  }
}

trait HasMemoryXilinxArtyA7MIGModuleImp extends LazyModuleImp
    with HasMemoryXilinxArtyA7MIGBundle {
  val outer: HasMemoryXilinxArtyA7MIG
  val ranges = AddressRange.fromSets(p(MemoryXilinxDDRKey).address)
  require (ranges.size == 1, "DDR range must be contiguous")
  val depth = ranges.head.size
  val xilinxartya7mig = IO(new XilinxArtyA7MIGIO(depth))

  xilinxartya7mig <> outer.xilinxartya7mig.module.io.port
}
