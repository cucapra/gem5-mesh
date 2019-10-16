#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "vvadd.h"
#include "../../common/bind_defs.h"

//#define VEC 1

// actual kernel
void kernel(
    float *a, float *b, float *c, int size, 
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // start recording all stats (all cores)
  // use the last thread, b/c this wakes up last?
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }
  
  #ifndef VEC
  for (int i = 0; i < size; i++) {
    c[i] = a[i] + b[i];
  }
  #else
  if (tid_x == 0 && tid_y == 0) {
    BINDED_FET_SOURCE(
      FET_O_INST_RIGHT_SEND,
      ALL_NORM,
        
      for (int i = 0; i < size; i++) {
        c[i] = a[i] + b[i];
      }
    );
  }
  else if (tid_x == 1 && tid_y == 0) {
    BINDED_FET_SOURCE(
      FET_I_INST_LEFT | FET_O_INST_DOWN_SEND,
      ALL_NORM,
        
      for (int i = 0; i < size; i++) {
        c[i] = a[i] + b[i];
      }
    );
  }
  else if (tid_x == 0 && tid_y == 1) {
    BINDED_FET_SOURCE(
      FET_I_INST_RIGHT,
      ALL_NORM,
        
      for (int i = 0; i < size; i++) {
        c[i] = a[i] + b[i];
      }
      
    );
  }
  else if (tid_x == 1 && tid_y == 1) {
    BINDED_FET_SOURCE(
      FET_I_INST_UP | FET_O_INST_LEFT_SEND,
      ALL_NORM,
        
      for (int i = 0; i < size; i++) {
        c[i] = a[i] + b[i];
      }
    );
  }
  
  // if doing this way then don't need each core to save a label, can
  // get rid of the savejmp instruction?
  // Need to have at least one subsequent instruction for label to be here
  label:
  // master and slave jump to different places without this nop, is compiler optimizing something?
    //asm volatile ("nop\t\n");
  
  #endif
  
  if (tid_x == 0 && tid_y == 0) {
    stats_off();
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
