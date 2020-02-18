#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "gemm.h"
#include "spad.h"
#include "../../common/bind_defs.h"

#define REGION_SIZE 16
#define NUM_REGIONS 32

#define VEC_PFETCH
#define OUT_SPAD

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



void gemm(float *a, float *b, float *c, int m, int n, int t, 
    int m_start, int m_end, int n_start, int n_end, int tid) {


  int a_cache_size = m_end-m_start;
  int b_cache_size = n_end-n_start;
  int c_block_size = a_cache_size*b_cache_size;


  int spadRegion = 0;
  float *spAddr = (float*)getSpAddr(tid,0);

  #ifdef OUT_SPAD
  float *sp_c = spAddr + NUM_REGIONS*REGION_SIZE;
  #endif
  
  for (int k = 0; k < t; k++){
    
    #ifdef VEC_PFETCH

    float* sp_a = spAddr + spadRegion*REGION_SIZE + 0;
    float* sp_b = spAddr + spadRegion*REGION_SIZE + a_cache_size;

    // fetch a in scratchpad
    for (int i = 0; i <a_cache_size ; i++) { 
      VPREFETCH(sp_a + i , a + _idx_(i+m_start,k,t), 0);
    }
    
    // fetch b in scratchpad
    for (int j = 0; j < b_cache_size; j++) { 
      VPREFETCH(sp_b + j , b + _idx_(k,j+n_start,n), 0);
    }
    #endif

    for (int i = 0; i < a_cache_size; i++) {
      for (int j = 0; j < b_cache_size; j++) {
        float a_,b_;
        #ifdef VEC_PFETCH
        LWSPEC(a_, sp_a+i, 0);
        LWSPEC(b_, sp_b+j, 0);
        #else
        a_ = a[_idx_(i+m_start,k,t)];
        b_ = b[_idx_(k,j+n_start,n)];
        #endif
        #ifdef OUT_SPAD
        sp_c[_idx_(i,j,b_cache_size)] += a_*b_;
        #else
        c[_idx_(i+m_start,j+n_start,n)] += a_*b_;
        #endif
      }
    }

    #ifdef VEC_PFETCH
    spadRegion = (spadRegion + 1) % NUM_REGIONS;
    REMEM(0);
    #endif

  }

  //store c in DRAM
  #ifdef OUT_SPAD
  for (int i = 0; i < a_cache_size; i++) {
      for (int j = 0; j < b_cache_size; j++) {
        STORE_NOACK(sp_c[_idx_(i,j,b_cache_size)],c + _idx_(i+m_start, j+n_start, n), 0);
      }
  }
  #endif
  

//Inner product 
/*
  for (int i = m_start; i < m_end; i++) {
    for (int j = n_start; j < n_end; j++) {
      for (int k = 0; k < t; k++) {
        c[_idx_(i, j, n)] += a[_idx_(i,k,t)] * b[_idx_(k, j, n)];
      }
    }
  }
*/

//Outer product
/*
for (int k = 0; k < t; k++){
  for (int i = m_start; i < m_end; i++) { 
      for (int j = n_start; j < n_end; j++) {
        c[_idx_(i, j, n)] += a[_idx_(i,k,t)] * b[_idx_(k, j, n)];
      }
  }
}
*/

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
  
  // figure out which block this thread should do
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


  int prefetchMask = (NUM_REGIONS << PREFETCH_NUM_REGION_SHAMT) | (REGION_SIZE << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);

  #ifndef _VEC
  gemm(a, b, c, m, n, t, m_start, m_end, n_start, n_end, tid);
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
  
  gemm(a, b, c, m, n, t, m_start, m_end, n_start, n_end, tid);
  
  VECTOR_EPOCH(ALL_NORM);
  
  #endif
  
  
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

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }
      
  return NULL;
}
