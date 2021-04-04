BENCHNAME := gesummv
default: $(BENCHNAME)

# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS ?= 16
EXTRA_FLAGS ?= -DVEC_LEN=4 -DLONGLINES -DINIT_FRAMES=0 -DACCUM_GRANULARITY=1
HB_ARGS ?= --options="32"
# DEBUG_FLAGS := --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := gesummv_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk