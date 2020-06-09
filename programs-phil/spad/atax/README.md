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

If you need scalar and vector codes:
```
ENV_N_SPS=16 make scalar
ENV_N_SPS=16 make vector
```
Copy the asm file in the directory renaming it to `atax_combined.s` and compile using:
```
ENV_N_SPS=16 make pass
ENV_N_SPS=16 make combine
ENV_N_SPS=16 make
```
To run simulation on gem5 with a square matrix of size `128` and same number of cores:
```
OUT=atax-vec0-128 SIZE=128 ENV_N_SPS=16 make run
```