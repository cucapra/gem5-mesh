RISCV_INSTALL= ~/hammerblade/riscv-docker/riscv/bin
RVCC=$(RISCV_INSTALL)/riscv64-unknown-elf-gcc

default: all

all: test-mod.c
	$(RVCC) test-mod.c -static -o test-asm
