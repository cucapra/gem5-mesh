# ATAX

The version currently defaults to VEC groups of `4x4`. 

## Manycore

The benchmark compiles and executes on 16 cores by default.
To change this explicitly, do this:

    make N_SPS=16

You can also change the matrix size (the default is 128) when executing. To run with a custom number of cores and matrix size:

    make run SIZE=128 N_SPS=16


## Vector

### Manual version (DEPRECATED)
Copy over the files from the folder `manual_compile_files`.

If you need scalar and vector asm files:
```
make scalar N_SPS=16
make vector N_SPS=16
```
Rename the asm file to `atax_combined.s` and compile using:
```
make pass N_SPS=16
make combine N_SPS=16
make N_SPS=16
```
To run simulation on gem5 with a square matrix of size `128` and same number of cores:
```
make run N_SPS=16 SIZE=128
```

### Trilliasm

Recommended usage:
```
make clean
make atax
make run
```
Pass in the flags for `make run` or change the Makefile for defaults.