BENCHNAME := fdtd2d
default: $(BENCHNAME)

# run small sim for test
PROB_SIZE := 128 128 10
N_SPS ?= 16
CL_LEN := 64
EXTRA_FLAGS ?= -DMANYCORE_PREFETCH -DCACHE_LINE_SIZE=$(CL_LEN)
HB_ARGS := --options="$(PROB_SIZE)" --cacheline_size=$(CL_LEN) --net-width=4
#GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := fdtd2d_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
