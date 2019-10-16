#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "vvadd.h"
#include "../../common/bind_defs.h"

// actual kernel
void kernel(
    float *a, float *b, float *c, int size, 
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // start recording all stats (all cores)
  // use the last thread, b/c this wakes up last?
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }
  
  #ifndef _VEC
  for (int i = 0; i < size; i++) {
    c[i] = a[i] + b[i];
  }
  #else
  // different groups of tiles, but within tiles do the same thing (HBIR would be useful here!)
  // TODO can make this less awkard to right if use csr to read from reg and incur and extra instruction
  // or two each time config (already takes like 10 cycles, whats 2 more!)
  
  // upper left corner is the master
  if (tid_x == 0 && tid_y == 0) {
    BINDED_FET_SOURCE(
      FET_O_INST_DOWN_SEND | FET_O_INST_RIGHT_SEND,
      ALL_NORM,
        
      for (int i = 0; i < size; i++) {
        c[i] = a[i] + b[i];
      }
    );
  }
  
  // right edge does not send to anyone
  else if (tid_x == dim_x - 1) {
    BINDED_FET_SOURCE(
      FET_I_INST_LEFT,
      ALL_NORM,
        
      for (int i = 0; i < size; i++) {
        c[i] = a[i] + b[i];
      }
    );
  }
  
  // bottom left corner just sends to the right
  else if (tid_x == 0 && tid_y == dim_y - 1) {
    BINDED_FET_SOURCE(
      FET_I_INST_UP | FET_O_INST_RIGHT_SEND,
      ALL_NORM,
        
      for (int i = 0; i < size; i++) {
        c[i] = a[i] + b[i];
      }
    );
  }
  
  // the left edge (besides corners) sends down and to the right
  else if (tid_x == 0) {
    BINDED_FET_SOURCE(
      FET_I_INST_UP | FET_O_INST_DOWN_SEND | FET_O_INST_RIGHT_SEND,
      ALL_NORM,
        
      for (int i = 0; i < size; i++) {
        c[i] = a[i] + b[i];
      }
    );
  }
  
  // otherwise we're just forwarding to the right in the middle area
  else {
    BINDED_FET_SOURCE(
      FET_I_INST_LEFT | FET_O_INST_RIGHT_SEND,
      ALL_NORM,
        
      for (int i = 0; i < size; i++) {
        c[i] = a[i] + b[i];
      }
    );
  }
  
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
