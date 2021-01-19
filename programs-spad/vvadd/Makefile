BENCHNAME := vvadd
default: $(BENCHNAME)

N_SPS ?= 16
EXTRA_FLAGS ?= -DVEC_4_SIMD_VERTICAL -DCACHE_LINE_SIZE=64
HB_ARGS := --options="64" --cacheline_size=64
GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=RiscvVector,Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := vvadd_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
