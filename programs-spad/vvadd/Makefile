BENCHNAME := vvadd
default: $(BENCHNAME)

N_SPS ?= 16

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := vvadd_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
