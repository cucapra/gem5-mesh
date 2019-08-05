/*
 * Host code
 */ 

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "pthread_launch.h"
#include "vec.h"

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
  
  
  /*--------------------------------------------------------------------
  * create package to pass to spmd kernel
  *-------------------------------------------------------------------*/
  
  // initialize the arguments to send to each device core
  vec_args_t **kern_args = (vec_args_t**)malloc(sizeof(vec_args_t*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      vec_args_t *ka = (vec_args_t*)malloc(sizeof(vec_args_t));
      
      // pack the relevant args
      ka->tid_x = x;
      ka->tid_y = y;
      ka->dim_x = cores_x;
      ka->dim_y = cores_y;
      
      kern_args[i] = ka;
    }  
  }
  
  /*--------------------------------------------------------------------
  * run the kernel on specified cores
  *-------------------------------------------------------------------*/
  
  launch_kernel(vec_pthread, (void**)kern_args, cores_x, cores_y);
  
  /*--------------------------------------------------------------------
  * inspect the output
  *-------------------------------------------------------------------*/
  
  int correct = 1;
  
  
  if (correct == 1) printf("[[SUCCESS]]\n");
  else printf("[[FAIL]]\n");
  
  /*--------------------------------------------------------------------
  * cleanup
  *-------------------------------------------------------------------*/
  
  
  return 0;
}
