BENCHNAME := 3mm
default: $(BENCHNAME)

# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS ?= 64
EXTRA_FLAGS ?= -DVECTOR_LEN=4 -DPER_CORE_SIMD
HB_ARGS ?= --options="256 256 256 256 256"
# DEBUG_FLAGS := --debug-flags=Mesh

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := gemm_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk