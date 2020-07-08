C_SRCS = $(wildcard *.c) $(wildcard ../common/*.c)
DEPS = $(C_SRCS:.c=.o)
C_OBJS = $(notdir $(C_SRCS:.c=.o))


EXEC = gemm

# defaults if environment variables not set

ifeq ($(ENV_N_SPS),)
	N_SPS = 4
else
	N_SPS = $(ENV_N_SPS)
endif	

EXTRA_FLAGS:= 

ifeq ($(ENV_NO_SP),)
	EXTRA_FLAGS := $(EXTRA_FLAGS) -D_BLOCKED
else
	EXTRA_FLAGS := $(EXTRA_FLAGS)
endif

ifeq ($(ENV_NO_VEC),)
	EXTRA_FLAGS := $(EXTRA_FLAGS) -D_VEC
else
	EXTRA_FLAGS := $(EXTRA_FLAGS)
endif

# installed cross compiler gcc for riscv
RV_CC=/data/phil/riscv-build/bin/riscv64-unknown-linux-gnu-gcc

CFLAGS=-D_N_SPS=$(N_SPS) $(EXTRA_FLAGS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm

default: $(EXEC)

$(EXEC): $(DEPS)
	$(RV_CC) $(C_OBJS) $(CFLAGS) -o $(EXEC)

%.o:%.c
	$(RV_CC) $(CFLAGS) -c -o $(notdir $@) $<

clean:
	rm -rf *.o $(EXEC)
