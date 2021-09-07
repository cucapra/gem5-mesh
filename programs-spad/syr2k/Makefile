BENCHNAME := syr2k
default: $(BENCHNAME)

# # run small sim for test
PROB_SIZE := 64
N_SPS ?= 16
CL_LEN := 64
EXTRA_FLAGS ?= -DNO_VEC -DCACHE_LINE_SIZE=$(CL_LEN)
HB_ARGS := --options="$(PROB_SIZE) $(PROB_SIZE)" --cacheline_size=$(CL_LEN)

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := syr2k_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
