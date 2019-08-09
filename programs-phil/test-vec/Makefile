SRCS=vec.c host.c ../common/pthread_launch.c

OBJS=$(SRCS:.c=.o)
LOCAL_OBJS=$(notdir $(SRCS:.c=.o))


# installed cross compiler gcc for riscv
CC=/home/pbb59/hammerblade/riscv-docker/riscv-build/bin/riscv64-unknown-linux-gnu-gcc

# need to compile with static in system emulation mode!
CFLAGS=-static -lpthread -lm -I../common/

default: vec-riscv asm-dump

vec-riscv: $(OBJS) 
	$(CC) $(LOCAL_OBJS) $(CFLAGS) -o test_vec

asm-dump: 
	$(CC) -c vec.c $(CFLAGS) -S -o test_vec_dump

%.o: %.c
	$(CC) $(CFLAGS) -c -o $(notdir $@) $<

clean:
	rm -rf test_vec test_vec_dump *.o
