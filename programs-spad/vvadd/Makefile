BENCHNAME := vvadd
default: $(BENCHNAME)

N_SPS ?= 64
EXTRA_FLAGS ?= -DVEC_4_SIMD
# HB_ARGS := --options="131072"
HB_ARGS := --options="8192"
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := vvadd_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
