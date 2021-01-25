BENCHNAME := vvadd
default: $(BENCHNAME)

N_SPS ?= 16
EXTRA_FLAGS ?= -DNO_VEC -DCACHE_LINE_SIZE=1024
HB_ARGS := --options="64" --cacheline_size=1024
# GEM5_ARGS ?= --remote-gdb-port=0 --debug-flags=SyscallBase

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := vvadd_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk
