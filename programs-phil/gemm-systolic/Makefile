SRCS=gemm.c host.c ../common/pthread_launch.c

OBJS=$(SRCS:.c=.o)
LOCAL_OBJS=$(notdir $(SRCS:.c=.o))


# installed cross compiler gcc for riscv
CC=/home/pbb59/hammerblade/riscv-docker/riscv-build/bin/riscv64-unknown-linux-gnu-gcc

# need to compile with static in system emulation mode!
CFLAGS=-static -lpthread -lm -I../common/

default: gemm-riscv

gemm-riscv: $(OBJS) 
	$(CC) $(LOCAL_OBJS) $(CFLAGS) -o gemm_systolic

%.o: %.c
	$(CC) $(CFLAGS) -c -o $(notdir $@) $<

clean:
	rm -rf gemm_systolic *.o
