C_SRCS = $(wildcard *.c) $(wildcard ../common/*.c)
DEPS = $(C_SRCS:.c=.o)
C_OBJS = $(notdir $(C_SRCS:.c=.o))
S_DEPS = $(C_SRCS:.c=.s)
S_OBJS = $(notdir $(C_SRCS:.c=.s))
# object files compiled via the ASM route
ASMO_DEPS = $(C_SRCS:.c=.O)
ASMO_OBJS = $(notdir $(C_SRCS:.c=.O))

SM_DEPS = $(C_SRCS:.c=.S)
SM_OBJS = $(notdir $(C_SRCS:.c=.S))


EXEC = gemm

# defaults if environment variables not set

ifeq ($(ENV_N_SPS),)
	N_SPS = 64
else
	N_SPS = $(ENV_N_SPS)
endif	

EXTRA_FLAGS:= 

ifeq ($(ENV_NO_SP),)
	EXTRA_FLAGS := $(EXTRA_FLAGS) 
else
	EXTRA_FLAGS := $(EXTRA_FLAGS)
endif

ifeq ($(ENV_NO_VEC),)
	EXTRA_FLAGS := $(EXTRA_FLAGS)
else
	EXTRA_FLAGS := $(EXTRA_FLAGS)
endif

ifneq ($(ENV_EXTRA_MAKE_FLAGS),)
	EXTRA_FLAGS := $(EXTRA_FLAGS) $(ENV_EXTRA_MAKE_FLAGS)
endif

ifeq ($(OUT),)
	OUT := gemm_default
endif
ifeq ($(SIZE),)
	SIZE := 32
endif



# installed cross compiler gcc for riscv
RV_CC=/data/phil/riscv-rv64g/bin/riscv64-unknown-linux-gnu-gcc

_GCC=gcc

# script to find the correct vissue length
VISS=python ../../../scripts-phil/vissue-asm.py


C_OBJS_FIL := $(filter-out $(EXEC)_kernel.o, $(C_OBJS))
DEPS_FIL := $(filter-out $(EXEC)_kernel.o, $(DEPS))

# C_OBJS_FIL = $(C_OBJS)
ifneq (,$(wildcard ./$(EXEC)_final.o))
	C_OBJS_FIL += $(EXEC)_final.o
	DEPS += $(EXEC)_final.o
endif

CFLAGS=-DSCALAR_CORE -D_N_SPS=$(N_SPS) $(EXTRA_FLAGS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm
vector: CFLAGS=-DVECTOR_CORE -D_N_SPS=$(N_SPS) $(EXTRA_FLAGS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm
scalar: CFLAGS=-DSCALAR_CORE -D_N_SPS=$(N_SPS) $(EXTRA_FLAGS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm

default: $(EXEC).neil

vector: $(EXEC)_kernel.c
	$(RV_CC) $(CFLAGS) -S -o $(EXEC)_vector.s $<

scalar: $(EXEC)_kernel.c
	$(RV_CC) $(CFLAGS) -S -o $(EXEC)_scalar.s $<

pass: $(EXEC)_combined.s
	$(VISS) $(EXEC)_combined.s $(EXEC)_final.s 1

combine: $(EXEC)_final.s
	$(RV_CC) $(CFLAGS) -c -o $(EXEC)_final.o $(notdir $<)


$(EXEC).neil: $(DEPS_FIL)
	$(RV_CC) $(C_OBJS_FIL) $(CFLAGS) -o $(EXEC)	

run: $(EXEC).neil
	../../../build/RVSP/gem5.opt -d ../../../results/$(OUT) \
	../../../configs/phil/brg_hammerblade.py --cmd=$(EXEC) --options="$(SIZE) $(SIZE) $(SIZE)" --num-cpus=$(ENV_N_SPS) --vector

# compile directly from .c to .o with no mods
$(EXEC).orig: $(DEPS)
	$(RV_CC) $(C_OBJS) $(CFLAGS) -o $(EXEC)

# compile first to asm so we can modify vissue insts and then compile to .o
$(EXEC): $(ASMO_DEPS)
	$(RV_CC) $(ASMO_OBJS) $(CFLAGS) -o $(EXEC)
	
# comile to original asm
ASM: $(S_DEPS)

# modify vissue insts in asm
VISS: $(SM_DEPS)

# %.O:%.S
# 	$(RV_CC) $(CFLAGS) -c -o $(notdir $@) $(notdir $<)

# %.S:%.s
# 	$(VISS) $(notdir $<) $(notdir $@)

%.s:%.c
	$(RV_CC) $(CFLAGS) -S -o $(notdir $@) $<

%.o:%.c
	$(RV_CC) $(CFLAGS) -c -o $(notdir $@) $<

clean:
	rm -rf *.o *.O *.s *.S $(EXEC) $(EXEC).orig