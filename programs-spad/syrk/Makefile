BENCHNAME := syrk
default: $(BENCHNAME)

# # run small sim for test
# PROB_SIZE := 96
N_SPS ?= 16
# HB_ARGS := --options="$(PROB_SIZE) $(PROB_SIZE)"

# Build the kernel with trilliasm.
#TRILLIASM_KERNEL := syrk_kernel.c
#include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
