C_SRCS = $(wildcard *.c) $(wildcard ../common/*.c)
DEPS = $(C_SRCS:.c=.o)
C_OBJS = $(notdir $(C_SRCS:.c=.o))


EXEC = sddmm

N_SPS = 16

# installed cross compiler gcc for riscv
RV_CC=/data/phil/riscv-build/bin/riscv64-unknown-linux-gnu-gcc

CFLAGS=-D_N_SPS=$(N_SPS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm

default: $(EXEC)

$(EXEC): $(DEPS)
	$(RV_CC) $(C_OBJS) $(CFLAGS) -o $(EXEC)

%.o:%.c
	$(RV_CC) $(CFLAGS) -c -o $(notdir $@) $<

clean:
	rm -rf *.o $(EXEC)
