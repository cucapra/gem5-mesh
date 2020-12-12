BENCHNAME := bfs
default: $(BENCHNAME)

# Specify number of CPUs. (TODO: rename to NUM_CPUS)
N_SPS ?= 16
EXTRA_FLAGS ?= -DVEC_LEN=4
HB_ARGS ?= --options="data/graph128.txt "
# DEBUG_FLAGS ?= --debug-flags=Mesh

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := bfs_kernel.c
include ../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk