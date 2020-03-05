#!/bin/bash
if [ $# -lt 1 ]; then
  echo "Usage: $0 [fp32|posit32|posit16|posit8]"
  exit 1
fi
XFIX=$1
if [[ "$XFIX" != "fp32" ]] && [[ "$XFIX" != "posit32" ]] && [[ "$XFIX" != "posit16" ]] && [[ "$XFIX" != "posit8" ]]; then
  echo "Usage: $0 [fp32|posit32|posit16|posit8]"
  exit 1
fi
rm -f rocket-chip
ln -s rocket-chip-$XFIX rocket-chip
cp configs/$XFIX-Config.scala src/main/scala/everywhere/e300artydevkit/Config.scala
export PATH=$PATH:$HOME/Tools/Xilinx/Vivado/2019.2/bin
make BOARD=arty_a7_100 -f Makefile.e300artydevkit clean
make BOARD=arty_a7_100 -f Makefile.e300artydevkit verilog
make BOARD=arty_a7_100 -f Makefile.e300artydevkit mcs
mkdir -p images/$XFIX
cp builds/e300artydevkit/obj/E300ArtyDevKitFPGAChip.mcs images/$XFIX/
