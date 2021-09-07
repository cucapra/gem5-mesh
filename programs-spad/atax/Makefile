BENCHNAME := atax
default: $(BENCHNAME)

SIZE ?= 64

N_SPS ?= 1
CL_LEN := 64
EXTRA_FLAGS ?= -DMANYCORE_PREFETCH -DCACHE_LINE_SIZE=$(CL_LEN)
# GEM5_ARGS := --debug-flags=Frame
HB_ARGS := --options="$(SIZE) $(SIZE)" --cacheline_size=$(CL_LEN)

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := atax_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
