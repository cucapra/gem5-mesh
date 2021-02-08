BENCHNAME := vvadd
default: $(BENCHNAME)

N_SPS ?= 64
EXTRA_FLAGS ?= -DVEC_16_SIMD_BIGBOI -DCACHE_LINE_SIZE=1024 -DINIT_FRAMES=1
HB_ARGS := --options="65536" --cacheline_size=1024 --net-width=1
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := vvadd_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
