BENCHNAME := bfs
default: $(BENCHNAME)

# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS ?= 16
HB_ARGS ?= --options="data/graph128.txt "

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := bfs_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk