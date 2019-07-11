/*
 * Host code
 */ 


#include <stdio.h>
#include <stdlib.h>
#include <assert.h>


#include "pthread_launch.h"
#include "gemm.h"

int main(int argc, char *argv[]) {
  // get dimensions of device manycore
  int cores_x, cores_y;
  get_dimensions(&cores_x, &cores_y);
  int num_cores = cores_x * cores_y;
  
  
  printf("Hello world! %d cores in device\n", num_cores);
  
  // initialize args
  int *a;
  int *b;
  int *c;
  int m;
  int n;
  int t;
  
  
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
  
  // run the kernel to completion
  launch_kernel(gemm_pthread, (void**)kern_args, cores_x, cores_y);
  
  return 0;
}
