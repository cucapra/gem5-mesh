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

EXEC = stencil

# defaults if environment variables not set

ifeq ($(ENV_N_SPS),)
	N_SPS = 64
else
	N_SPS = $(ENV_N_SPS)
endif	

EXTRA_FLAGS:= 

ifneq ($(ENV_EXTRA_MAKE_FLAGS),)
	EXTRA_FLAGS := $(EXTRA_FLAGS) $(ENV_EXTRA_MAKE_FLAGS)
endif

# installed cross compiler gcc for riscv
RV_CC=/data/phil/riscv-rv64g/bin/riscv64-unknown-linux-gnu-gcc

# script to find the correct vissue length
VISS=python /home/pbb59/hammerblade/gem5/scripts-phil/vissue-asm.py

CFLAGS=-D_N_SPS=$(N_SPS) $(EXTRA_FLAGS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm

default: $(EXEC)

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

%.O:%.S
	$(RV_CC) $(CFLAGS) -c -o $(notdir $@) $(notdir $<)

%.S:%.s
	$(VISS) $(notdir $<) $(notdir $@) 1

%.s:%.c
	$(RV_CC) $(CFLAGS) -S -o $(notdir $@) $<

%.o:%.c
	$(RV_CC) $(CFLAGS) -c -o $(notdir $@) $<

clean:
	rm -rf *.o *.O *.s *.S $(EXEC) $(EXEC).orig
