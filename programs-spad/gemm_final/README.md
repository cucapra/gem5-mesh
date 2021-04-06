# GEMM

# Algo

`C=alpha.A.B+beta.C`

## Runing gemm

Use `gemm.h` to select `_VEC` flag for vector mode or manycore mode. We can also change `VECTOR_LEN` which takes either `4` or `16`.
`Makefile` can be used to edit the number of cores and size of matrix.

`make clean`
`make run` 



