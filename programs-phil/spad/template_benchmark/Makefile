BENCHNAME:=template
default: $(BENCHNAME)

# plug in trilliasm make recipes
TRILLIASM_KERNEL = template_kernel.c
TRILLIUM_DIR=../../../trillium
include $(TRILLIUM_DIR)/trilliasm.mk

# run standard compilation
COMMON_DIR=../common
include $(COMMON_DIR)/common.mk

# specify number of cpus (TODO: rename to NUM_CPUS)
N_SPS=16
