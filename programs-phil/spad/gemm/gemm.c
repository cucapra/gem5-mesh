#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "gemm.h"
#include "../../common/bind_defs.h"

static inline int _idx_(int y, int x, int width) {
  return (y * width) + x;
}

static inline int get_blk_end(int iter, int bound, int blk_dim) {
  int iter_next = iter + blk_dim;
  if (iter_next > bound) {
    return bound;
  }
  else {
    return iter_next;
  }
}

static inline void gemm_vonneumann(float *a, float *b, float *c, int m, int n, int t, 
    int m_start, int m_end, int n_start, int n_end, int blk_dim) {
  
#ifndef _BLOCKED

  for (int j = m_start; j < m_end; j++) { 
    for (int i = n_start; i < n_end; i++) {
      for (int k = 0; k < t; k++) {
        c[_idx_(j, i, n)] += a[_idx_(j, k, t)] * b[_idx_(k, i, n)];
      }
    }
  }
  
#else

  int j0_max = m_end;
  for (int j0 = m_start; j0 < j0_max; j0+=blk_dim) {
    
    // make sure the block doesn't go out of allocated bounds
    int j1_max = get_blk_end(j0, j0_max, blk_dim);
     
    int i0_max = n_end;
    for (int i0 = n_start; i0 < i0_max; i0+=blk_dim) {
      
      // make sure x direction doesnt go out of bounds
      int i1_max = get_blk_end(i0, i0_max, blk_dim);
      
      for (int k0 = 0; k0 < t; k0+=blk_dim) {
        
        int k1_max = get_blk_end(k0, t, blk_dim);
        
        // block to increase spatial locality within private cache
        // currenlty try to hit in cache line, but spads dont have lines?
        // alternively could make it the size of the entire cache
        for (int j1 = j0; j1 < j1_max; j1++) {
          
          // these inner loops are across a single cache line
          for (int i1 = i0; i1 < i1_max; i1++) {
            for (int k1 = k0; k1 < k1_max; k1++) {
              
              c[_idx_(j1, i1, n)] += a[_idx_(j1, k1, t)] * b[_idx_(k1, i1, n)];
              
            }
          }
        }
        
        
        
      }
    }
  }

#endif
}

// actual kernel
void kernel(
    float *a, float *b, float *c, int m, int n, int t, int blk_dim,
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // figure out which work this thread should do
  int m_start = tid_y * (m / dim_y);
  int n_start = tid_x * (n / dim_x);  

  // get end with remainders
  int m_chunk = (int)(m / dim_y);
  int n_chunk = (int)(n / dim_x);
  if (tid_x == dim_x - 1) {
    n_chunk += n % dim_x;
  }
  if (tid_y == dim_y - 1) {
    m_chunk += m % dim_y;
  }
  int m_end = m_start + m_chunk;
  int n_end = n_start + n_chunk;
  
  // start recording all stats (all cores)
  // use the last thread, b/c this wakes up last?
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }
  
  #ifndef _VEC
  gemm_vonneumann(a, b, c, m, n, t, m_start, m_end, n_start, n_end, blk_dim);
  #else
  // gemm_vonneumann
  // gemm_vonneumann
  // gemm_vonneumann
  #endif
  
  if (tid_x == 0 && tid_y == 0) {
    stats_off();
  }
  
}


// helper functions
Kern_Args *construct_args(float *a, float *b, float *c, int m, int n, int t,
  int blk_dim,
  int tid_x, int tid_y, int dim_x, int dim_y) {
      
  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->c = c;
  args->m = m;
  args->n = n;
  args->t = t;
  args->blk_dim = blk_dim; // unused in basic version
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
  
  kernel(a->a, a->b, a->c, a->m, a->n, a->t, a->blk_dim,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);
      
  return NULL;
}
