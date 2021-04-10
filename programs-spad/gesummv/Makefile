BENCHNAME := gesummv
default: $(BENCHNAME)

# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS ?= 16
CL_LEN := 1024
EXTRA_FLAGS ?= -DVECTOR_LEN=4 -DPER_CORE_SIMD -DLONGLINES -DCACHE_LINE_SIZE=$(CL_LEN)
HB_ARGS ?= --options="64" --cacheline_size=$(CL_LEN)
# DEBUG_FLAGS := --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := gesummv_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk