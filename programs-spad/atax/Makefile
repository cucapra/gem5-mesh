BENCHNAME := atax
default: $(BENCHNAME)

SIZE ?= 128
OUT ?= $(EXEC)-vec0-$(SIZE)

N_SPS ?= 64  # This benchmark expects 16 CPUs for now.
EXTRA_FLAGS ?=-DNESTED_SIMD -DVEC_LEN=4 -DHARDWARE_VECTOR_LEN=4
# GEM5_ARGS := --debug-flags=Frame #-d ../../results/$(OUT)
HB_ARGS := --options="$(SIZE) $(SIZE)"

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := atax_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
