#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "gemm.h"
#include "spad.h"
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

// issue where getting a 'c_jr' (indirect jump) in this function that is messing up vectorization
// this function is getting a 'call memset' emitted as a part of gcc loop-tree-distrubute optimizations?
// https://stackoverflow.com/questions/6410595/getting-gcc-to-compile-without-inserting-call-to-memcpy
// TODO The baseline should still have this optimization enabled??
//static inline
//void __attribute__((optimize("-fno-tree-loop-distribute-patterns")))
void
  gemm_vonneumann(float *a, float *b, float *c, int m, int n, int t, 
    int m_start, int m_end, int n_start, int n_end, int blk_dim, int tid) {
  
#ifndef _BLOCKED

  for (int j = m_start; j < m_end; j++) { 
    for (int i = n_start; i < n_end; i++) {
      for (int k = 0; k < t; k++) {
        c[_idx_(j, i, n)] += a[_idx_(j, k, t)] * b[_idx_(k, i, n)];
      }
    }
  }
  
#else
  // first 3 loops, loop over blocks
  // last 3 loops, do mat mul within the blocks

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
        
        // mini matmul to only access data in the local scratchpad
        // TODO for now don't interleave compute
        // TODO also stack is sent to global memory not scratchpad, that needs to be fixed (hacktober?), somewhere in riscv/process or sim/process
        // does a stack not make sense in spad architectures?
        // TODO this call is broken for offset other than 0 (should just have get getSpadBasePtr());
        float *base = getSpAddr(tid, 0); 
        float *asp = &(base[0]);
        float *bsp = &(base[blk_dim * blk_dim]);
        float *csp = &(base[2 * blk_dim * blk_dim]);
        int mblk = j1_max - j0;
        int nblk = i1_max - i0;
        int tblk = k1_max - k0;
        
        for (int j1 = 0; j1 < mblk; j1++) {
          for (int i1 = 0; i1 < nblk; i1++) {
            csp[_idx_(j1, i1, nblk)] = 0;
          }
        }
        
        for (int j1 = 0; j1 < mblk; j1++) {
          for (int k1 = 0; k1 < tblk; k1++) {
            asp[_idx_(j1, k1, tblk)] = a[_idx_(j1 + j0, k1 + k0, t)];
          }
        }
        
        for (int i1 = 0; i1 < nblk; i1++) {
          for (int k1 = 0; k1 < tblk; k1++) {
            bsp[_idx_(k1, i1, nblk)] = b[_idx_(k1 + k0, i1 + i0, n)];
          }
        }
        
        // do the actual compute
        for (int j1 = 0; j1 < mblk; j1++) {
          for (int i1 = 0; i1 < nblk; i1++) {
            for (int k1 = 0; k1 < tblk; k1++) {
              
              //c[_idx_(j1, i1, n)] += a[_idx_(j1, k1, t)] * b[_idx_(k1, i1, n)];
              csp[_idx_(j1, i1, nblk)] += asp[_idx_(j1, k1, tblk)] * bsp[_idx_(k1, i1, nblk)];
            }
          }
        }
        
        // accumulate scratchpad results into main memory
        for (int j1 = 0; j1 < mblk; j1++) {
          for (int i1 = 0; i1 < nblk; i1++) {
            c[_idx_(j1 + j0, i1 + i0, n)] += csp[_idx_(j1, i1, nblk)];
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
  
  // start recording all stats (all cores)
  // use the last thread, b/c this wakes up last?
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }
  
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
  
  int tid = tid_x + tid_y * dim_x;
  
  //printf("iterations %d %d\n", n_end - n_start, m_end - m_start);
  
  #ifndef _VEC
  gemm_vonneumann(a, b, c, m, n, t, m_start, m_end, n_start, n_end, blk_dim, tid);
  #else
  int mask = ALL_NORM;
  
  // upper left corner is the master
  if (tid_x == 0 && tid_y == 0) {
    mask = FET_O_INST_DOWN_SEND | FET_O_INST_RIGHT_SEND;
  }
  
  // right edge does not send to anyone
  else if (tid_x == dim_x - 1) {
    mask = FET_I_INST_LEFT;
  }
  
  // bottom left corner just sends to the right
  else if (tid_x == 0 && tid_y == dim_y - 1) {
    mask = FET_I_INST_UP | FET_O_INST_RIGHT_SEND;
  }
  
  // the left edge (besides corners) sends down and to the right
  else if (tid_x == 0) {
    mask = FET_I_INST_UP | FET_O_INST_DOWN_SEND | FET_O_INST_RIGHT_SEND;
  }
  
  // otherwise we're just forwarding to the right in the middle area
  else {
    mask = FET_I_INST_LEFT | FET_O_INST_RIGHT_SEND;
  }
  
  VECTOR_EPOCH(mask);
  
  gemm_vonneumann(a, b, c, m, n, t, m_start, m_end, n_start, n_end, blk_dim, tid);
  
  VECTOR_EPOCH(ALL_NORM);
  
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
