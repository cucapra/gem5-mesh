BENCHNAME := 2mm
default: $(BENCHNAME)

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := 2mm_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk

# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS := 16
HB_ARGS := --options="64 64 64"
# DEBUG_FLAGS := --debug-flags=Mesh
EXTRA_FLAGS := -DMM