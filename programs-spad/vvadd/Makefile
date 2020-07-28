BENCHNAME := vvadd
default: $(BENCHNAME)

N_SPS ?= 16
EXTRA_FLAGS ?= -DVEC_4_SIMD

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := vvadd_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
