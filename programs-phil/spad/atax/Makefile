#plug in trilliasm make recipes
TRILLIASM_KERNEL = atax_kernel.c
TRILLIUM_DIR=../../../trillium
RV_CC=/data/phil/riscv-rv64g/bin/riscv64-unknown-linux-gnu-gcc	

EXEC=atax
N_SPS?=16
SIZE?=128
OUT?=$(EXEC)-vec0-$(SIZE)
CFLAGS=-D_N_SPS=$(N_SPS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm
include $(TRILLIUM_DIR)/trilliasm.mk



C_SRCS_NOKERN := main.c $(EXEC).c $(wildcard ../common/*.c)
C_OBJS_NOKERN := $(C_SRCS_NOKERN:.c=.o)

$(EXEC): $(TRILLIASM_KERNEL:.c=.o) $(C_OBJS_NOKERN)
		$(RV_CC) $^ $(CFLAGS) -o $@

#--debug-flags=Mesh
run: $(EXEC)
		../../../build/RVSP/gem5.opt -d ../../../results/$(OUT) \
		../../../configs/phil/brg_hammerblade.py \
		--cmd=$(EXEC) \
		--options="$(SIZE) $(SIZE)" \
		--num-cpus=$(N_SPS) \
		--vector

# Static Pattern: only applies to files listed in C_OBJS_NOKERN
$(C_OBJS_NOKERN): %.o : %.c
		$(RV_CC) $(CFLAGS) -c $^ -o $(notdir $@)

clean:
		rm -rf *.o *.s $(EXEC) m5out
