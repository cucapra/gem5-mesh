BENCHNAME := gesummv
default: $(BENCHNAME)

# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS ?= 16
EXTRA_FLAGS ?= -DVECTOR_LEN=4 -DLONGLINES -DPER_CORE_SIMD
HB_ARGS ?= --options="64"
# DEBUG_FLAGS := --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := gesummv_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk