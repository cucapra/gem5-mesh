BENCHNAME := atax
default: $(BENCHNAME)

SIZE ?= 128
OUT ?= $(EXEC)-vec0-$(SIZE)

N_SPS ?= 16  # This benchmark expects 16 CPUs for now.
GEM5_ARGS := -d ../../results/$(OUT)
HB_ARGS := --options="$(SIZE) $(SIZE)"

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := atax_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
