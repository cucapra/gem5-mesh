BENCHNAME := atax
default: $(BENCHNAME)

SIZE ?= 128
OUT ?= $(EXEC)-vec0-$(SIZE)

N_SPS ?= 16  # This benchmark expects 16 CPUs for now.
GEM5_ARGS := -d ../../../results/$(OUT)
HB_ARGS := --options="$(SIZE) $(SIZE)"

#plug in trilliasm make recipes
TRILLIASM_KERNEL = atax_kernel.c
TRILLIUM_DIR=../../../trillium
include $(TRILLIUM_DIR)/trilliasm.mk

# run standard compilation
include ../common/common.mk