CPP_SRCS = $(wildcard *.cpp)
CPP_OBJS = $(CPP_SRCS:.cpp=.o)

# gem5 xcel directory to get shared types
#GEM5_XCEL = ../../src/accelerators

# installed cross compiler gcc for riscv
RV_CC=/home/pbb59/hammerblade/riscv-docker/riscv-build/bin/riscv64-unknown-linux-gnu-g++

# -D_N_CPUS=1 -D_N_XCELS=16
CFLAGS=-O3 --std=c++11 -static -D_N_CPUS=16 -T../common/spm.ld -lpthread

default: sddmm-cpu

sddmm-cpu: $(CPP_OBJS)
	$(RV_CC) $^ $(CFLAGS) -o sddmm_cpu

%.o:%.cpp
	$(RV_CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -rf *.o sddmm_cpu
