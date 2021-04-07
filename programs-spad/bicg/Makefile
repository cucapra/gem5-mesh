BENCHNAME := bicg
default: $(BENCHNAME)

# run small sim for test
PROB_SIZE := 16
N_SPS ?= 16
EXTRA_FLAGS ?= -DVECTOR_LEN=4
HB_ARGS := --options="$(PROB_SIZE) $(PROB_SIZE)"
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=RiscvVector

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := bicg_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
