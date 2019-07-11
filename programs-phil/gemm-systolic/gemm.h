#ifndef __GEMM_H__
#define __GEMM_H__


// data passed to each gemm thread
typedef struct gemm_args_t {
  // info about the system
  int tid_x, tid_y;
  int dim_x, dim_y;
  
  // gemm arrays and dimensions
  int m, n, t;
  int *a;
  int *b;
  int *c;
  
  // constructor
  /*gemm_args_t (int tid_x, int tid_y, int dim_x, int dim_y, int *a,
      int *b, int *c, int m, int n, int t) {
    this->tid_x = tid_x;
    this->tid_y = tid_y;
    this->dim_x = dim_x;
    this->dim_y = dim_y;
    this->m     = m;
    this->n     = n;
    this->t     = t;
    this->a     = a;
    this->b     = b;
    this->c     = c;
  }*/
  
} gemm_args_t;

// function pointer to launch with pthreads
void *gemm_pthread(void* args);

// actual spmd kernel
void gemm(int tid_x, int tid_y, int dim_x, int dim_y, int *a,
      int *b, int *c, int m, int n, int t);

#endif
