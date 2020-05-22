# VVADD SIMD

Build Instructions:
`make` builds `vvadd`
`make run` builds and runs `vvadd` on gem5 simulator


Benchmark description:
`main.c` calls `vvadd.c`, which in turn sets up arguments for vvadd kernel in `vvadd_kernel.c`.
`vvadd_kernel.c` contains vvadd kernel written in Vector-SIMCv1 style (lang docs forthcoming).


Compiling Vector-SIMCv1 consists of the following steps:
1. Run compiler over kernel file twice: once with `SCALAR_CORE` pragma, again with `VECTOR_CORE` pragma.
	- this generates `vvadd_scalar.s` and `vvadd_vector.s`
2. Run gluer (in `trillium` folder), which combines both assembly files into `vvadd_kernel.s`
3. Compile the kernel assembly file to an object file
4. Compile non-kernel files and link with kernel object file to generate `vvadd`


Future:
`vvadd_kernel.c` will be written in Vector-SIMCv2: a pragma-style C language for writing Vector-SIMD kernels.
