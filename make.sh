#!/bin/bash
if [ $# -lt 1 ]; then
  echo "Usage: $0 [fp32|posit32|posit16|posit8|fp32xmem|posit32xmem|posit16xmem|posit8xmem]"
  exit 1
fi
XFIX=$1
if [[ "$XFIX" != "fp32" ]] && [[ "$XFIX" != "posit32" ]] && [[ "$XFIX" != "posit16" ]] && [[ "$XFIX" != "posit8" ]] \
&& [[ "$XFIX" != "fp32xmem" ]] && [[ "$XFIX" != "posit32xmem" ]] && [[ "$XFIX" != "posit16xmem" ]] && [[ "$XFIX" != "posit8xmem" ]]; then
  echo "Usage: $0 [fp32|posit32|posit16|posit8|fp32xmem|posit32xmem|posit16xmem|posit8xmem]"
  exit 1
fi
SRC_XFIX=`echo $XFIX | cut -d 'x' -f 1`

rm -f rocket-chip
ln -s rocket-chip-$SRC_XFIX rocket-chip
cp configs/$XFIX-Config.scala src/main/scala/everywhere/e300artydevkit/Config.scala
export PATH=$PATH:$HOME/Tools/Xilinx/Vivado/2019.2/bin
make BOARD=arty_a7_100 -f Makefile.e300artydevkit clean
make BOARD=arty_a7_100 -f Makefile.e300artydevkit verilog
make BOARD=arty_a7_100 -f Makefile.e300artydevkit mcs
mkdir -p images/$XFIX
cp builds/e300artydevkit/obj/E300ArtyDevKitFPGAChip.mcs images/$XFIX/
mkdir -p logs/$XFIX
cp -r builds/e300artydevkit logs/$XFIX/
