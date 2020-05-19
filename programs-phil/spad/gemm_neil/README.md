# GEMM

The most important benchmark in the history of vector architectures? Debatable. But we sure know how to run it, thanks to this amazingly easy to follow documentation.

GEMM has 2 broad versions - `NO_VEC` which runs as a regular many core program on `n` cores. This version has non blocked and blocked versions to serve as baselines. The other version is `_VEC` which works by grouping these cores together in a SIMD fashion. For this version we are implementing blocked GEMM, where each vector core works on a block/tile of the output matrix and computes it using _outer product_. Since we can group these cores in different sizes of groups and use different memory sharing models among these vector cores we have different versions within `_VEC`. These are described below:

1. Memory model based version: `SIMD_PRIVATE` and `SIMD_SHARING` are the two versions we have for GEMM. `SIMD_PRIVATE` has every vector core read its private Scratchpad only. `SIMD_SHARING`, as the name suggests, shares scratchpad space among groups of vector cores. So each vector core has access to scratchpad of all the vector cores in a group. This enables amortization of loading a value once from main memory, which can be used by all the cores in a group but pays the price of slightly slower access to remote scratchpad locations. 
2. Group size based versions: We support vector lengths of `4` and `16` currently, for atl east `16` and `64` total cores correspondingly. 

Why do you need this information? We use flags while compiling the codes based on the version of GEMM you are running. We'll discuss the flags needed to compile using `make` commands. 

## make flags
The flags to use while to represent all the versions are:
```
SIMD_PRIVATE_4
SIMD_PRIVATE_16
SIMD_SHARING_4
SIMD_SHARING_16
```

## Assembly files

To understand the assembly files refer to `../vvadd_simd/` documentation. The only assembly file in the folder is `*_combined.s` which is currently hand tuned.

## Runing gemm

All that is okay and I don't really care about what is happening inside the kernel. How do I run GEMM?

Based on the versions discussed above, copy the assembly file `gemm_combined.s` of the corresponding version using
```
cp asm_<private/sharing>_float<4/16>/* .
```
For eg, if you want to run `SIMD_PRIVATE` with groups of `16` vector cores:
```
cp asm_private_float16/* .
```

Now compile the `gemm_kernel` using:
```
make combine
```

Then compile the kernel with the following flags. Use one of the version flags from [here](#make-flags). Make sure you also pass the total cores using `ENV_N_SPS=64`. Remember for  `16` group size, we need at least 64 cores in the mesh. Hence we use:
```
ENV_EXTRA_MAKE_FLAGS=-DSIMD_PRIVATE_16 ENV_N_SPS=64 make
```
To run it on gem5 use the command `make run` and use the variable assignment for output address of the stats, size of matrix and total cores to simulate on:
```
OUT=gemm-vec1-SIMD_PRIVATE-size_mnt32 SIZE=32 ENV_N_SPS=64 make run
```

And as all always, please make sure to clean the mess you created :)
```
make clean
```



