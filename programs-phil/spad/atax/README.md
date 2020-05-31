# ATAX

The version currently defaults to VEC groups of `4x4`. 
To compile with `16` cores we use:
```
ENV_N_SPS=16 make
```
To run simulation on gem5 with a square matrix if size `128` and same number of cores:
```
OUT=atax-vec0-128 SIZE=128 ENV_N_SPS=16 make run
```