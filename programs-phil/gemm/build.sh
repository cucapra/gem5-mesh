#!/bin/bash

make clean
# x86 riscv
export ARCH=$1
# opt debug
export DEBUG=$2
make
