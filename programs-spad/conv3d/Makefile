BENCHNAME := conv3d
default: $(BENCHNAME)

# run small sim for test
PROB_SIZE := 12 12 10
N_SPS ?= 64
HB_ARGS := --options="$(PROB_SIZE)"
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := conv3d_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
