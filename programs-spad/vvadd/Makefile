BENCHNAME := vvadd
default: $(BENCHNAME)

N_SPS ?= 1
EXTRA_FLAGS ?= -DNO_VEC
HB_ARGS := --options="65536" --num-cpus=1 --cpu-type=DerivO3CPU
GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=Frame,Scratchpad

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := vvadd_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
