BENCHNAME:=vvadd
default: $(BENCHNAME)

#plug in trilliasm make recipes
TRILLIASM_KERNEL = vvadd_kernel.c
TRILLIUM_DIR=../../../trillium
include $(TRILLIUM_DIR)/trilliasm.mk

# run standard compilation
COMMON_DIR=../common
include $(COMMON_DIR)/common.mk
