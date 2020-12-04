BENCHNAME := vvadd
default: $(BENCHNAME)

N_SPS ?= 16
EXTRA_FLAGS ?= -DPACKED_SIMD
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=RiscvVector,Mesh

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := vvadd_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
