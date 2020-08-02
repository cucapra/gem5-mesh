BENCHNAME := covar
default: $(BENCHNAME)

# run small sim for test
PROB_SIZE := 16
N_SPS ?= 64
EXTRA_FLAGS ?= -DVEC_16_SIMD
HB_ARGS := --options="$(PROB_SIZE) $(PROB_SIZE)"

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := covar_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
