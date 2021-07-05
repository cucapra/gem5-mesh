BENCHNAME := gesummv
default: $(BENCHNAME)

# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS ?= 16
CL_LEN := 64
EXTRA_FLAGS ?= -DMANYCORE_PREFETCH -DCACHE_LINE_SIZE=$(CL_LEN)
HB_ARGS ?= --options="256" --cacheline_size=$(CL_LEN) --net-width=32
# DEBUG_FLAGS := --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := gesummv_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
