BENCHNAME := conv2d
default: $(BENCHNAME)

# run small sim for test
# PROB_SIZE := 96
N_SPS ?= 16
# HB_ARGS := --options="$(PROB_SIZE) $(PROB_SIZE)"
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=Mesh

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := conv2d_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
