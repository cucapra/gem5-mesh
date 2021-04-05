BENCHNAME := syrk
default: $(BENCHNAME)

# run small sim for test
# inner dim needs to be >=64 for longlines to work
PROB_SIZE := 64
N_SPS ?= 16
CL_LEN := 256
# CL_LEN := 64
EXTRA_FLAGS ?= -DVECTOR_LEN=4 -DPER_CORE_SIMD -DCACHE_LINE_SIZE=$(CL_LEN)
HB_ARGS := --options="$(PROB_SIZE) $(PROB_SIZE)" --cacheline_size=$(CL_LEN) --net-width=16
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := syrk_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
