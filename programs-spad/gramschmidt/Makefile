BENCHNAME := gramschmidt
default: $(BENCHNAME)

# run small sim for test
PROB_SIZE := 32
N_SPS ?= 16
EXTRA_FLAGS ?=-DNO_VEC
HB_ARGS := --options="$(PROB_SIZE) $(PROB_SIZE)"
# GEM5_ARGS := --remote-gdb-port=0 --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := gram_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
