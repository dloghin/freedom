#!/bin/bash
make BOARD=arty_a7_100 -f Makefile.e300artydevkit clean
make BOARD=arty_a7_100 -f Makefile.e300artydevkit verilog
make BOARD=arty_a7_100 -f Makefile.e300artydevkit mcs
