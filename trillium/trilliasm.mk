#This Makefile fragment generates rules for building a Trilliasm kernel.
#To use, you must specify the name of the Trilliasm kernel file:
#TRILLIASM_KERNEL= name of the trilliasm kernel used in benchmark (e.g.: vvadd_kernel.c)

#In addition, `trilliasm.mk` needs the following bindings, for which defaults exist:
#TRILLIUM_DIR= relative path to trillium directory (default: ../../../trillium)
#COMMON_PATH= relative path to `common` folder with benchmark dependencies
#RV_CC= path to RV compiler (default: /data/phil/riscv-rv64g/bin/riscv64-unknown-linux-gnu-gcc)
#CFLAGS= flags to pass to RV_CC 

#Then, include `trilliasm.mk` at the top of your Makefile:
#include $(TRILLIUM_DIR)/trilliasm.mk

#NOTE: to avoid clashing (%.o) targets with the $(KERNEL_NAME).o target, replace rules of the form:
# %.o: %.c   with   $(MY_NONKERNEL_OBJ_FILES) : %.o: %.c
#(i.e. Static Pattern Rules: https://www.gnu.org/software/make/manual/html_node/Static-Usage.html#Static-Usage)

#NOTE: `trilliasm.mk` internally uses KERNEL_NAME to strip the path and file extension from TRILLIASM_KERNEL
#I'm not good enough at Makefiles yet to know for sure if you can break `trilliasm.mk` by rebinding that variable, so beware

TRILLIUM_DIR?=../../../trillium
COMMON_PATH?=../common
RV_CC?=/data/phil/riscv-rv64g/bin/riscv64-unknown-linux-gnu-gcc
CFLAGS?=-D_N_SPS=16 -O3 --std=gnu11 -static -I$(COMMON_PATH) -T$(COMMON_PATH)/spm.ld -lpthread -lm


KERNEL_NAME:= $(basename $(TRILLIASM_KERNEL))
TRILLIASM_OBJS=$(TRILLIASM_KERNEL:.c=.o)

$(KERNEL_NAME)_vector.s: $(KERNEL_NAME).c
	$(RV_CC) $(CFLAGS) -D VECTOR_CORE -S $< -o $@

$(KERNEL_NAME)_scalar.s: $(KERNEL_NAME).c
	$(RV_CC) $(CFLAGS) -D SCALAR_CORE -S $< -o $@ 

$(KERNEL_NAME).s: $(KERNEL_NAME)_vector.s $(KERNEL_NAME)_scalar.s
	python3 $(TRILLIUM_DIR)/glue.py $^ -o $(KERNEL_NAME).s

$(KERNEL_NAME).o: $(KERNEL_NAME).s
	$(RV_CC) $(CFLAGS) -c $^ -o $@
