#plug in trilliasm make recipes
TRILLIASM_KERNEL = vvadd_kernel.c
TRILLIUM_DIR=../../../trillium
RV_CC=/data/phil/riscv-rv64g/bin/riscv64-unknown-linux-gnu-gcc
CFLAGS=-D_N_SPS=16 -DVEC_4_SIMD -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm

include $(TRILLIUM_DIR)/trilliasm.mk

C_SRCS_NOKERN := main.c vvadd.c $(wildcard ../common/*.c)
C_OBJS_NOKERN := $(C_SRCS_NOKERN:.c=.o)

# installed cross compiler gcc for riscv
vvadd : $(TRILLIASM_KERNEL:.c=.o) $(C_OBJS_NOKERN)
	$(RV_CC) $^ $(CFLAGS) -o $@

run : vvadd
	~/gem5-mesh/build/RVSP/gem5.opt ~/gem5-mesh/configs/phil/brg_hammerblade.py \
	--cmd=vvadd \
	--options="1024" \
	--num-cpus=16 \
	--vector

$(C_OBJS_NOKERN): %.o : %.c
	$(RV_CC) $(CFLAGS) -c $^ -o $@

clean:
	rm -rf *.o *.s vvadd m5out
