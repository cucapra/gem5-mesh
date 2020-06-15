# VVADD SIMD

Build Instructions:
`make vvadd` to build gem5 benchmark
`make run` to build and run gem5 benchmark

Benchmark description:
`main.c` calls `vvadd.c`, which in turn sets up arguments for vvadd kernel in `vvadd_kernel.c`.
`vvadd_kernel.c` contains a Trilliasm vvadd kernel (lang docs forthcoming).


Compiling Trilliasm consists of the following steps:
1. Run compiler over kernel file twice: once with `SCALAR_CORE` pragma, again with `VECTOR_CORE` pragma.
	- this generates `vvadd_scalar.s` and `vvadd_vector.s`
2. Run gluer, which combines both assembly files into `vvadd_kernel.s`
3. Compile the kernel assembly file to an object file `vvadd_kernek.o`
4. Compile non-kernel files and link with kernel object file to generate `vvadd`

The Make include `trilliasm.mk` performs steps 1-3, whereas the local Makefile performs step 4

Future:
Port to Trilligma
