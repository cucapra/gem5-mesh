# This Makefile fragment inclues rules for building a Trilliasm kernel.
# To use, you must specify the name of the Trilliasm kernel source file,
# and then include this file:
#
#     TRILLIASM_KERNEL := mybench_kernel.c
# 	  include ../../../trillium/trillium.mk
#
# Also be sure to define RV_CC and CFLAGS for the compilation rules.
#
# NOTE: to avoid clashing (%.o) targets with the $(KERNEL_NAME).o target,
# replace rules of the form:
# 	%.o: %.c
# with:
#  $(MY_NONKERNEL_OBJ_FILES): %.o: %.c
# Those are "static pattern rules":
# https://www.gnu.org/software/make/manual/html_node/Static-Usage.html#Static-Usage

# Ensure that RV_CC and CFLAGS are set (we do not define them here). These
# extra flags apply only to compiling the vector "half" of the code.
VECTOR_CFLAGS ?= -fno-reorder-blocks

TRILLIUM_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
KERNEL_NAME := $(basename $(TRILLIASM_KERNEL))
TRILLIASM_OBJS := $(TRILLIASM_KERNEL:.c=.o)

$(KERNEL_NAME)_vector.s: $(KERNEL_NAME).c
	$(RV_CC) $(VECTOR_CFLAGS) $(CFLAGS) -D VECTOR_CORE -S $< -o $@

$(KERNEL_NAME)_scalar.s: $(KERNEL_NAME).c
	$(RV_CC)  $(VECTOR_CFLAGS) $(CFLAGS) -D SCALAR_CORE -S $< -o $@ 

$(KERNEL_NAME).s: $(KERNEL_NAME)_vector.s $(KERNEL_NAME)_scalar.s
	python3 $(TRILLIUM_DIR)/glue.py $^ -o $(KERNEL_NAME).s

$(KERNEL_NAME).o: $(KERNEL_NAME).s
	$(RV_CC) $(CFLAGS) -c $^ -o $@
