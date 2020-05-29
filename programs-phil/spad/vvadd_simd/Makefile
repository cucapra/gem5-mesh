EXEC = vvadd

SRC_COMMON := $(wildcard ../common/*.c)
SRC_MAIN   := main.c $(EXEC).c
SRC_KERNEL := $(EXEC)_kernel.c

C_SRCS        := $(SRC_MAIN) $(SRC_KERNEL) $(SRC_COMMON)
C_SRCS_NOKERN := $(SRC_MAIN) $(SRC_COMMON)

C_OBJS        := $(C_SRCS:.c=.o)
C_OBJS_NOKERN := $(C_SRCS_NOKERN:.c=.o)

# defaults if environment variables not set

ENV_N_SPS=16 
ifeq ($(ENV_N_SPS),)
N_SPS = 16
else
N_SPS = $(ENV_N_SPS)
endif	

EXTRA_FLAGS:= -DVEC_4_SIMD

ifneq ($(ENV_EXTRA_MAKE_FLAGS),)
	EXTRA_FLAGS := $(EXTRA_FLAGS) $(ENV_EXTRA_MAKE_FLAGS)
endif

# installed cross compiler gcc for riscv
RV_CC=/data/phil/riscv-rv64g/bin/riscv64-unknown-linux-gnu-gcc

CFLAGS=-D_N_SPS=$(N_SPS) $(EXTRA_FLAGS) -O3 --std=gnu11 -static -I../common/ -T../common/spm.ld -lpthread -lm

default: $(EXEC)

#$(EXEC)_kernel.c: $(EXEC)_kernel.tr
#	stack trillium/Splitter.hs

$(EXEC)_vector.s: $(EXEC)_kernel.c
	$(RV_CC) $(CFLAGS) -D VECTOR_CORE -S $< -o $@

$(EXEC)_scalar.s: $(EXEC)_kernel.c
	$(RV_CC) $(CFLAGS) -D SCALAR_CORE -S $< -o $@ 

$(EXEC)_kernel.s: $(EXEC)_vector.s $(EXEC)_scalar.s
	python3 trillium/glue.py $^ -o $@
	
$(EXEC)_kernel.o: $(EXEC)_kernel.s
	$(RV_CC) $(CFLAGS) -c $< -o $(notdir $@)

$(EXEC): $(C_OBJS_NOKERN) $(EXEC)_kernel.o
	$(RV_CC) $(notdir $^) $(CFLAGS) -o $(EXEC)

run : $(EXEC)
	~/gem5-mesh/build/RVSP/gem5.opt ~/gem5-mesh/configs/phil/brg_hammerblade.py \
	--cmd=vvadd \
	--options="1024" \
	--num-cpus=16 \
	--vector

%.s:%.c
	$(RV_CC) $(CFLAGS) -S -o $(notdir $@) $<

%.o:%.c
	$(RV_CC) $(CFLAGS) -c $< -o $(notdir $@)

clean:
	rm -rf *.o *.O *.s *.S $(EXEC) $(EXEC).orig


