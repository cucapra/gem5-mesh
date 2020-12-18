BENCHNAME := 3mm
default: $(BENCHNAME)

# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS ?= 16
EXTRA_FLAGS ?= -DPACKED_SIMD
HB_ARGS ?= --options="16 16 16 16 16"
# DEBUG_FLAGS := --debug-flags=Mesh

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := gemm_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk