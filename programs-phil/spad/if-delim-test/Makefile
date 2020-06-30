BENCHNAME:=template
default: $(BENCHNAME)

# plug in trilliasm make recipes
TRILLIASM_KERNEL = if_test_kernel.c
TRILLIUM_DIR=../../../trillium
include $(TRILLIUM_DIR)/trilliasm.mk

# run standard compilation
include ../common/common.mk

# specify number of cpus (TODO: rename to NUM_CPUS)
N_SPS=16
