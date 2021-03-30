BENCHNAME := covar
default: $(BENCHNAME)

# run small sim for test
PROB_SIZE := 32
N_SPS ?= 64
CL_LEN := 128
EXTRA_FLAGS ?= -DVEC_16_LONGLINES -DCACHE_LINE_SIZE=$(CL_LEN)
HB_ARGS := --options="$(PROB_SIZE) $(PROB_SIZE)" --cacheline_size=$(CL_LEN)
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=Mesh,Frame

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := covar_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
