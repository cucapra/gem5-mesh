BENCHNAME := gemm
default: $(BENCHNAME)

SIZE := 80
# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS ?= 64
EXTRA_FLAGS ?= -DNO_VEC
HB_ARGS := --options="$(SIZE) $(SIZE) $(SIZE)"
# DEBUG_FLAGS := --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := gemm_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk

