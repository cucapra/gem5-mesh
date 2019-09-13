#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "vvadd.h"

// actual kernel
void kernel(
    float *a, float *b, float *c, int size, 
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  
  for (int i = 0; i < size; i++) {
    c[i] = a[i] + b[i];
  }
  
}


// helper functions
Kern_Args *construct_args(float *a, float *b, float *c, int size,
  int tid_x, int tid_y, int dim_x, int dim_y) {
      
  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->c = c;
  args->size = size;
  args->tid_x = tid_x;
  args->tid_y = tid_y;
  args->dim_x = dim_x;
  args->dim_y = dim_y;
  
  return args;
      
}

void *pthread_kernel(void *args) {
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early
  pthread_barrier_wait(&start_barrier);
  
  // call the spmd kernel
  Kern_Args *a = (Kern_Args*)args;
  
  kernel(a->a, a->b, a->c, a->size, 
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);
      
  return NULL;
}
