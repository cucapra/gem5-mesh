#ifndef __GEMM_KERNELS_H__
#define __GEMM_KERNELS_H__

#include <pthread.h>
#include <iostream>

void *gemm(void *args);

typedef struct gemm_args_t {
	int *a;
	int *b;
	int *c;
	int cid_x;
  int cid_y;
  int cores_x;
	int cores_y;
	int m, n, t;

	gemm_args_t (int *a, int *b, int *c, int cid_x, int cid_y, int cores_x, 
            int cores_y, int m, int n, int t) {
		this->a = a;
		this->b = b;
		this->c = c;
    this->cid_x = cid_x;
    this->cid_y = cid_y;
		this->cores_x = cores_x;
    this->cores_y = cores_y;
		this->m = m;
    this->n = n;
    this->t = t;
	}
} gemm_args_t;

extern "C" pthread_barrier_t start_barrier;
#endif
