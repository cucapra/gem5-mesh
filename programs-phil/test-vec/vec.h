#ifndef __VEC_H__
#define __VEC_H__


// data passed to each gemm thread
typedef struct vec_args_t {
  // info about the system
  int tid_x, tid_y;
  int dim_x, dim_y;
} vec_args_t;

// function pointer to launch with pthreads
void *vec_pthread(void* args);

// actual spmd kernel
void vec(int tid_x, int tid_y, int dim_x, int dim_y);

#endif
