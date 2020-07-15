BENCHNAME := gemm
default: $(BENCHNAME)

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := gemm_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk

# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS := 16
HB_ARGS := --options="128"
# DEBUG_FLAGS := --debug-flags=Mesh