BENCHNAME := conv2d
default: $(BENCHNAME)

# run small sim for test
PROB_SIZE := 64
N_SPS ?= 64
CL_LEN := 64
EXTRA_FLAGS ?= -DVEC_16_SIMD -DCACHE_LINE_SIZE=$(CL_LEN)
HB_ARGS := --options="$(PROB_SIZE) $(PROB_SIZE)" --cacheline_size=$(CL_LEN)
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := conv2d_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
