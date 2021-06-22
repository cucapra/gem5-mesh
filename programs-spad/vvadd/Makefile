BENCHNAME := vvadd
default: $(BENCHNAME)

N_SPS ?= 64
EXTRA_FLAGS ?= -DNO_VEC
HB_ARGS := --options="65536" --cpu-type=DerivO3CPU
GEM5_ARGS ?= --remote-gdb-port=0

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := vvadd_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
