BENCHNAME := gemm
default: $(BENCHNAME)

SIZE := 16
# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS ?= 16
EXTRA_FLAGS ?= -DPACKED_SIMD
HB_ARGS := --options="$(SIZE) $(SIZE) $(SIZE)"
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=RiscvVector

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := gemm_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk

