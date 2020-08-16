BENCHNAME := gemm
default: $(BENCHNAME)

SIZE := 64
# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS ?= 64
EXTRA_FLAGS ?= -DVEC_LEN=4
HB_ARGS := --options="$(SIZE) $(SIZE) $(SIZE)"
# DEBUG_FLAGS := --debug-flags=Mesh

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := gemm_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk

