BENCHNAME := fdtd2d
default: $(BENCHNAME)

# run small sim for test
PROB_SIZE := 32 32 2
N_SPS ?= 16
EXTRA_FLAGS ?= -DPACKED_SIMD
HB_ARGS := --options="$(PROB_SIZE)"
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=RiscvVector

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := fdtd2d_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
