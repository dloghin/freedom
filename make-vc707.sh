#!/bin/bash

if [ $# -lt 1 ]; then
  echo "Usage: $0 [fp32|posit32|posit16|posit8|big]"
  exit 1
fi
XFIX=$1
if [[ "$XFIX" != "fp32" ]] && [[ "$XFIX" != "posit32" ]] && [[ "$XFIX" != "posit16" ]] && [[ "$XFIX" != "posit8" ]] \
&& [[ "$XFIX" != "big" ]]; then
  echo "Usage: $0 [fp32|posit32|posit16|posit8|big]"
  exit 1
fi
SRC_XFIX=`echo $XFIX | cut -d 'x' -f 1`

rm -f rocket-chip
ln -s rocket-chip-$SRC_XFIX rocket-chip

cd fpga-shells
git checkout .
git apply ../configs-vc707/vc707_lcd_jtag.patch
cd ..
cp configs-vc707/U500-DevKitConfig-1Core-$XFIX.scala src/main/scala/unleashed/DevKitConfigs.scala
export PATH=$PATH:$HOME/Tools/Xilinx/Vivado/2016.4/bin
export RISCV="/home/dumi/git/riscv/riscv-toolchain/riscv64-unknown-elf-gcc-8.3.0-2019.08.0-x86_64-linux-ubuntu14"
make MODEL=VC707BaseShell -f Makefile.vc707-u500devkit clean
make MODEL=VC707BaseShell -f Makefile.vc707-u500devkit verilog
make MODEL=VC707BaseShell -f Makefile.vc707-u500devkit mcs
mkdir -p images-vc707/$XFIX
rm -r images-vc707/$XFIX/*
cp builds/vc707-u500devkit/obj/VC707BaseShell.mcs images-vc707/$XFIX/
cp builds/vc707-u500devkit/obj/VC707BaseShell.prm images-vc707/$XFIX/
cp builds/vc707-u500devkit/sifive.freedom.unleashed.DevKitU500FPGADesign_WithDevKit50MHz.dts images-vc707/$XFIX/
mkdir -p logs-vc707/$XFIX
rm -r logs-vc707/$XFIX/*
cp -r builds/vc707-u500devkit logs-vc707/$XFIX/

