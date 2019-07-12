/*
 * Host code
 */ 

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "pthread_launch.h"
#include "gemm.h"

int main(int argc, char *argv[]) {
  
  /*--------------------------------------------------------------------
  * Get the dimensions of the manycore
  *-------------------------------------------------------------------*/
  
  int cores_x, cores_y;
  get_dimensions(&cores_x, &cores_y);
  int num_cores = cores_x * cores_y;
  
  
  printf("Hello world! %d cores in device\n", num_cores);
  
  /*--------------------------------------------------------------------
  * initialize args
  *-------------------------------------------------------------------*/
  
  // dimensions
  int m = 2;
  int n = 2;
  int t = 2;
  
  // declared y by x dimensions
  int *a = (int*)malloc(m * t);
  int *b = (int*)malloc(t * n);
  int *c = (int*)malloc(m * n);
  
  // init data in matrices
  for (int j = 0; j < m; j++) {
    for (int k = 0; k < t; k++) {
      a[j * m + k] = 1;
    }
  }
  
  for (int k = 0; k < t; k++) {
    for (int i = 0; i < n; i++) {
      b[k * t + i] = 1;
    }
  }
  
  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      c[j * m + i] = 0;
    }
  }
  
  /*--------------------------------------------------------------------
  * create package to pass to spmd kernel
  *-------------------------------------------------------------------*/
  
  // initialize the arguments to send to each device core
  gemm_args_t **kern_args = (gemm_args_t**)malloc(sizeof(gemm_args_t*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      gemm_args_t *ka = (gemm_args_t*)malloc(sizeof(gemm_args_t));
      
      // pack the relevant args
      ka->tid_x = x;
      ka->tid_y = y;
      ka->dim_x = cores_x;
      ka->dim_y = cores_y;
      ka->a     = a;
      ka->b     = b;
      ka->c     = c;
      ka->m     = m;
      ka->n     = n;
      ka->t     = t;
      
      kern_args[i] = ka;
    }  
  }
  
  /*--------------------------------------------------------------------
  * run the kernel on specified cores
  *-------------------------------------------------------------------*/
  
  launch_kernel(gemm_pthread, (void**)kern_args, cores_x, cores_y);
  
  /*--------------------------------------------------------------------
  * inspect the output
  *-------------------------------------------------------------------*/
  
  printf("Output matrix c\n");
  
  int correct = 1;
  
  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      int idx = j * m + i;
      printf("%d\t", c[idx]);
      if (c[idx] != t) correct = 0; 
    }
    printf("\n");
  }
  
  if (correct == 1) printf("[[SUCCESS]]\n");
  else printf("[[FAIL]]\n");
  
  /*--------------------------------------------------------------------
  * cleanup
  *-------------------------------------------------------------------*/
  
  free(a);
  free(b);
  free(c);
  
  return 0;
}
