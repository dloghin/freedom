#!/bin/bash

if ! [ -d "fpga-shells" ]; then
	echo "fpga-shells dir is missing!"
	exit 1
fi
cd fpga-shells
git checkout .
git apply ../configs-vc707/vc707_lcd_jtag.patch
cd ..
cp configs-vc707/U500-DevKitConfig-1Core.scala src/main/scala/unleashed/DevKitConfigs.scala
make MODEL=VC707BaseShell -f Makefile.vc707-u500devkit clean
make MODEL=VC707BaseShell -f Makefile.vc707-u500devkit verilog
make MODEL=VC707BaseShell -f Makefile.vc707-u500devkit mcs
