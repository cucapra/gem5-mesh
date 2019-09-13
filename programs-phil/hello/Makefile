# use the local x86 gcc. don't need to cross compile since using x86 gem5
X86_CC=gcc
# installed cross compiler gcc for riscv
RV_CC=/data/phil/riscv-build/bin/riscv64-unknown-linux-gnu-g++

# need to compile with static in system emulation mode!
CFLAGS=-static

default: hello-x86 hello-riscv

hello-x86:
	$(X86_CC) hello.c $(CFLAGS) -o hello_x86

hello-riscv:
	$(RV_CC) hello.c $(CFLAGS) -o hello_riscv

clean:
	rm -rf hello_x86 hello_riscv
