BENCHNAME := vvadd
default: $(BENCHNAME)

N_SPS ?= 64
EXTRA_FLAGS ?= -DVEC_4_SIMD
HB_ARGS := --options="131072"

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := vvadd_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
