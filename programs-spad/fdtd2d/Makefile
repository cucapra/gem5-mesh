BENCHNAME := fdtd2d
default: $(BENCHNAME)

# run small sim for test
PROB_SIZE := 512 512 30
N_SPS ?= 64
CL_LEN := 1024
EXTRA_FLAGS ?= -DVECTOR_LEN=16 -DLONGLINES -DPER_CORE_SIMD -DCACHE_LINE_SIZE=$(CL_LEN)
HB_ARGS := --options="$(PROB_SIZE)" --cacheline_size=$(CL_LEN) --net-width=32
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=RiscvVector

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := fdtd2d_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
