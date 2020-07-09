BENCHNAME := mvt
default: $(BENCHNAME)

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := mvt_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk

# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS := 64
HB_ARGS := --options="128"
DEBUG_FLAGS := --debug-flags=Mesh