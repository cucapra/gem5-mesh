# defaults if environment variables not set. allows scritable command line

N_SPS ?= 64
# ifneq ($(ENV_N_SPS),)
# 	N_SPS ?= $(ENV_N_SPS)
# endif

EXTRA_FLAGS ?=
# ifneq ($(ENV_EXTRA_MAKE_FLAGS),)
# 	EXTRA_FLAGS := $(EXTRA_FLAGS) $(ENV_EXTRA_MAKE_FLAGS)
# endif

# Overridable arguments to the simulation command for `make run`.
GEM5_ARGS ?= --remote-gdb-port=0
HB_ARGS ?= --options=""

# unique binary id to differentiate
BINARY_NAME ?= $(BENCHNAME) 

# Find the repository's base directory.
COMMON_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
BASE_DIR := $(COMMON_DIR)/../..

# installed cross compiler gcc for riscv
RV_CC=/scratch/pbb59/rv64g/bin/riscv64-unknown-linux-gnu-gcc

CFLAGS=-D_N_SPS=$(N_SPS) $(EXTRA_FLAGS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm

C_SRCS_NOKERN := $(filter-out $(TRILLIASM_KERNEL), $(wildcard *.c))
C_DEPS_NOKERN := $(C_SRCS_NOKERN:.c=.o)

# Build common libraries, but build them "locally" (in this directory) so
# they get updated according to this benchmark's configuration.
COMMON_SRCS := $(wildcard $(COMMON_DIR)/*.c)
COMMON_OBJS := $(notdir $(COMMON_SRCS:.c=.o))

$(BENCHNAME) : $(TRILLIASM_OBJS) $(C_DEPS_NOKERN) $(COMMON_OBJS)
	$(RV_CC) $(TRILLIASM_OBJS) $(C_DEPS_NOKERN) $(COMMON_OBJS) $(CFLAGS) -o $(BINARY_NAME)

run: $(BENCHNAME)
	$(BASE_DIR)/build/RVSP/gem5.opt \
		$(DEBUG_FLAGS) \
		$(GEM5_ARGS) \
		$(BASE_DIR)/configs/phil/brg_hammerblade.py \
		--cmd=$(BENCHNAME) \
		$(HB_ARGS) \
		--num-cpus=$(N_SPS) \
		--vector

$(C_DEPS_NOKERN): %.o: %.c
	$(RV_CC) $(CFLAGS) -c $^ -o $@

$(COMMON_OBJS): %.o: $(COMMON_DIR)/%.c
	$(RV_CC) $(CFLAGS) -c $^ -o $@

clean:
	rm -rf *.o *.s $(BINARY_NAME) m5out
