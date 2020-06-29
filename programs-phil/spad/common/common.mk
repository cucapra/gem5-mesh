# defaults if environment variables not set. allows scritable command line

ifeq ($(ENV_N_SPS),)
	N_SPS = 64
else
	N_SPS = $(ENV_N_SPS)
endif	

EXTRA_FLAGS:= 

ifneq ($(ENV_EXTRA_MAKE_FLAGS),)
	EXTRA_FLAGS := $(EXTRA_FLAGS) $(ENV_EXTRA_MAKE_FLAGS)
endif

# Find the repository's base directory.
COMMON_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
BASE_DIR := $(COMMON_DIR)/../../..

# installed cross compiler gcc for riscv
RV_CC=/data/phil/riscv-rv64g/bin/riscv64-unknown-linux-gnu-gcc

CFLAGS=-D_N_SPS=$(N_SPS) $(EXTRA_FLAGS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm

C_SRCS_NOKERN := $(filter-out $(TRILLIASM_KERNEL), $(wildcard *.c)) $(wildcard ../common/*.c)
C_DEPS_NOKERN := $(C_SRCS_NOKERN:.c=.o)
C_OBJS_NOKERN := $(notdir $(C_SRCS_NOKERN:.c=.o))

$(BENCHNAME) : $(TRILLIASM_OBJS) $(C_DEPS_NOKERN)
	$(RV_CC) $(TRILLIASM_OBJS) $(C_OBJS_NOKERN) $(CFLAGS) -o $@

run: $(BENCHNAME)
	$(BASE_DIR)/build/RVSP/gem5.opt \
	--remote-gdb-port=0 \
	$(BASE_DIR)/configs/phil/brg_hammerblade.py \
	--cmd=$(BENCHNAME) \
	--options="" \
	--num-cpus=$(N_SPS) \
	--vector

$(C_DEPS_NOKERN): %.o : %.c
	$(RV_CC) $(CFLAGS) -c $^ -o $(notdir $@)

clean:
	rm -rf *.o *.s $(BENCHNAME) m5out