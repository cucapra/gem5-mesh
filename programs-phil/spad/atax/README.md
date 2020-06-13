# ATAX

The version currently defaults to VEC groups of `4x4`. 

## Manycore
To compile with `16` cores we use:
```
ENV_N_SPS=16 make
```
To run simulation on gem5 with a square matrix of size `128` and same number of cores:
```
OUT=atax-vec0-128 SIZE=128 ENV_N_SPS=16 make run
```

## Vector

### Manual version (DEPRECATED)
Copy over the files from the folder `manual_compile_files`.

If you need scalar and vector asm files:
```
ENV_N_SPS=16 make scalar
ENV_N_SPS=16 make vector
```
Rename the asm file to `atax_combined.s` and compile using:
```
ENV_N_SPS=16 make pass
ENV_N_SPS=16 make combine
ENV_N_SPS=16 make
```
To run simulation on gem5 with a square matrix of size `128` and same number of cores:
```
OUT=atax-vec0-128 SIZE=128 ENV_N_SPS=16 make run
```

### Trilliasm

Recommended usage:
```
make clean
make atax
make run
```
Pass in the flags for `make run` or change the Makefile for defaults.