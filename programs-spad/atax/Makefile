BENCHNAME := atax
default: $(BENCHNAME)

SIZE ?= 64
OUT ?= $(EXEC)-vec0-$(SIZE)

N_SPS ?= 16  # This benchmark expects 16 CPUs for now.
CL_LEN := 256
EXTRA_FLAGS ?= -DVECTOR_LEN=4 -DLONGLINES -DCACHE_LINE_SIZE=$(CL_LEN)
# GEM5_ARGS := --debug-flags=Frame #-d ../../results/$(OUT)
HB_ARGS := --options="$(SIZE) $(SIZE)" --cacheline_size=$(CL_LEN)

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := atax_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
