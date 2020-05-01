C_SRCS = $(wildcard *.c) $(wildcard ../common/*.c)
DEPS = $(C_SRCS:.c=.o)
DEPS_FIL := $(filter-out $(EXEC)_kernel.o, $(DEPS))
C_OBJS = $(notdir $(C_SRCS:.c=.o))
S_DEPS = $(C_SRCS:.c=.s)
S_OBJS = $(notdir $(C_SRCS:.c=.s))
# object files compiled via the ASM route
ASMO_DEPS = $(C_SRCS:.c=.O)
ASMO_OBJS = $(notdir $(C_SRCS:.c=.O))

SM_DEPS = $(C_SRCS:.c=.S)
SM_OBJS = $(notdir $(C_SRCS:.c=.S))

EXEC = vvadd

# defaults if environment variables not set

ifeq ($(ENV_N_SPS),)
	N_SPS = 16
else
	N_SPS = $(ENV_N_SPS)
endif	

EXTRA_FLAGS:= 

ifeq ($(ENV_NO_SP),)
	EXTRA_FLAGS := $(EXTRA_FLAGS) -D_USE_SCRATCHPAD
else
	EXTRA_FLAGS := $(EXTRA_FLAGS)
endif

ifeq ($(ENV_NO_VEC),)
	EXTRA_FLAGS := $(EXTRA_FLAGS) -D_VEC
else
	EXTRA_FLAGS := $(EXTRA_FLAGS)
endif

ifneq ($(ENV_EXTRA_MAKE_FLAGS),)
	EXTRA_FLAGS := $(EXTRA_FLAGS) $(ENV_EXTRA_MAKE_FLAGS)
endif

# installed cross compiler gcc for riscv
RV_CC=/data/phil/riscv-rv64g/bin/riscv64-unknown-linux-gnu-gcc

# script to find the correct vissue length
VISS=python ../../../scripts-phil/vissue-asm.py

#in case of vector and scalar compilations
C_OBJS_FIL := $(filter-out vvadd_kernel.o, $(C_OBJS))
# C_OBJS_FIL = $(C_OBJS)
C_OBJS_FIL += $(EXEC)_final.o

CFLAGS=-DSCALAR_CORE -D_N_SPS=$(N_SPS) $(EXTRA_FLAGS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm
vector: CFLAGS=-DVECTOR_CORE -D_N_SPS=$(N_SPS) $(EXTRA_FLAGS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm
scalar: CFLAGS=-DSCALAR_CORE -D_N_SPS=$(N_SPS) $(EXTRA_FLAGS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm

default: $(EXEC).neil

vector: $(EXEC)_kernel.c
	$(RV_CC) $(CFLAGS) -S -o $(EXEC)_vector.s $<

scalar: $(EXEC)_kernel.c
	$(RV_CC) $(CFLAGS) -S -o $(EXEC)_scalar.s $<

1ruleforall: $(EXEC)_kernel.c
	$(RV_CC) -DVECTOR_CORE $(CFLAGS) -S -o $(EXEC)_vector.s $<
	$(RV_CC) -DSCALAR_CORE $(CFLAGS) -S -o $(EXEC)_scalar.s $<

pass: $(EXEC)_combined.s
	$(VISS) $(EXEC)_combined.s $(EXEC)_final.s 1

combine: $(EXEC)_final.s
	$(RV_CC) $(CFLAGS) -c -o $(EXEC)_final.o $(notdir $<)

# compile directly from .c to .o with no mods
$(EXEC).orig: $(DEPS)
	$(RV_CC) $(C_OBJS) $(CFLAGS) -o $(EXEC)

$(EXEC).neil: $(DEPS_FIL)
	$(RV_CC) $(C_OBJS_FIL) $(CFLAGS) -o $(EXEC)

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
