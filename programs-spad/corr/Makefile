BENCHNAME := corr
default: $(BENCHNAME)

# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS ?= 16
EXTRA_FLAGS ?= -DVECTOR_LEN=4 -DPER_CORE_SIMD
# EXTRA_FLAGS ?= -DNO_VEC
HB_ARGS := --options="32 32"
# DEBUG_FLAGS := --debug-flags=Mesh

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := corr_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk