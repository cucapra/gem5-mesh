COMMON_FLAGS= -static -pthread -std=c++11
OPT_FLAGS= -O3
DEBUG_FLAGS= -g -DDEBUG=1
# x86 riscv
#ARCH=x86
GCC=g++
RV_PATH=/home/pbb59/hammerblade/riscv-docker/riscv-build/bin
RVCC=$(RV_PATH)/riscv64-unknown-linux-gnu-g++


#DEBUG=

ifeq ($(DEBUG),opt)
  CPP_FLAGS = $(COMMON_FLAGS) $(OPT_FLAGS)
else
  CPP_FLAGS = $(COMMON_FLAGS) $(DEBUG_FLAGS)
endif

ifeq ($(ARCH),x86)
  CC=$(GCC)
else
  CC=$(RVCC)
endif

default: basic blocked

basic: gemm.o gemm_basic.o
	$(CC) $(CPP_FLAGS) -o basic_$(ARCH) gemm.o gemm_basic.o

blocked: gemm.o gemm_blocked.o
	$(CC) $(CPP_FLAGS) -o blocked_$(ARCH) gemm.o gemm_blocked.o

%.o: %.cpp
	$(CC) $(CPP_FLAGS) -c -o $@ $<

clean: 
	rm -rf basic* blocked* *.o
