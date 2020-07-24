BENCHNAME := 3mm
default: $(BENCHNAME)

# Specify number of CPUs. (TODO: rename to NUM_CPUS). With 64 CPUs might need to reduce prefetching 
N_SPS ?= 64
HB_ARGS ?= --options="32 32 32 32 32"
# DEBUG_FLAGS := --debug-flags=Mesh

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := gemm_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk