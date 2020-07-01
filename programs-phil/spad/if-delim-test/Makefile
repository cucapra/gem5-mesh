BENCHNAME := if_test
default: $(BENCHNAME)

# Build the kernel with trilliasm.
TRILLIASM_KERNEL := if_test_kernel.c
include ../../../trillium/trilliasm.mk

# Standard benchmark compilation.
include ../common/common.mk

# specify number of cpus (TODO: rename to NUM_CPUS)
N_SPS=16
