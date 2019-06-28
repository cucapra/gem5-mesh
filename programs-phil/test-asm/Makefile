# use the local x86 gcc. don't need to cross compile since using x86 gem5
X86_CC=gcc
# installed cross compiler gcc for riscv
RV_CC=/home/pbb59/hammerblade/riscv-docker/riscv-build/bin/riscv64-unknown-linux-gnu-gcc

# need to compile with static in system emulation mode!
CFLAGS=-static -lpthread -lm

default: asm-riscv asm-dump

asm-riscv:
	$(RV_CC) test_asm.c $(CFLAGS) -o test_asm_riscv

asm-dump:
	$(RV_CC) test_asm.c -lpthread -lm -S -o test_asm_rv_dump

clean:
	rm -rf test_asm_riscv test_asm_rv_dump
