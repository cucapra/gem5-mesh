BENCHNAME := syrk
default: $(BENCHNAME)

# # run small sim for test
PROB_SIZE := 32
N_SPS ?= 64
EXTRA_FLAGS ?= -DVEC_4_SIMD
HB_ARGS := --options="$(PROB_SIZE) $(PROB_SIZE)"
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := syrk_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
