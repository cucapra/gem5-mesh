#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "gemm.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"

#include "gemm_kernel.h"

void transpose_manycore(DTYPE *a, int a_row, int a_col, DTYPE *aT, int ptid, int pdim){

  int start = (ptid + 0) * a_col / pdim;
  int end = (ptid + 1) * a_col / pdim;

  for(int i=start; i<end; i++){
    for(int j=0; j<a_row; j++){
      // aT[i*a_row+j] = a[j*a_col+i];
      FSTORE_NOACK(a[j*a_col+i], &aT[i*a_row+j], 0);
    }
  }
  FENCE();
}

static inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

// actual kernel
void kernel(
    DTYPE *a, DTYPE *aT, DTYPE *b, DTYPE *c, int m, int n, int t,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y)
{

  // start recording all stats (all cores)
  // use the last thread, b/c this wakes up last?
  if (ptid_x == 0 && ptid_y == 0)
  {
    stats_on();
  }

  int m_start = 0;
  int m_end = 0;
  int n_start = 0;
  int n_end = 0;

  

  #ifdef _VEC

  #if VECTOR_LEN==4
  SET_USEFUL_VARIABLES_V4(ptid_x, ptid_y, pdim_x, pdim_y);
  #elif VECTOR_LEN==16
  SET_USEFUL_VARIABLES_V16(ptid_x, ptid_y, pdim_x, pdim_y);
  #endif

  WORK_DIV(m,n)

  // if (ptid==38) printf("mvec %d m manycore %d, weighted avergae %d\n",m_vec,m_manycore,(cinfo.total_groups*VECTOR_LEN*m)/total_compute_cores);
  // if(cinfo.used && cinfo.is_scalar) printf("used ptid:%d, start=%d end=%d\n",ptid,m_start,m_end);
  // if(!cinfo.used) printf("ptid:%d, start=%d end=%d\n",ptid,m_start,m_end);
  
#else
  SET_USEFUL_VARIABLES_MANYCORE(ptid_x, ptid_y, pdim_x, pdim_y);
  
  //do work division here

  m_start  = ptid_y * BLK_DIM;
  n_start  = ptid_x * BLK_DIM;
#endif

  transpose_manycore(a,m,t,aT,ptid,pdim);
  // a=aT;

// need to set vlen here so doesn't cause squash in vector core on change in value
  #ifdef PER_CORE_SIMD
  vsetvl_e32m1(HARDWARE_VECTOR_LEN);
  #endif

  // region based mask for scratchpad
#ifdef _VEC
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
#elif defined MANYCORE_PREFETCH
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
#else
  // need to wait after transpose!
  pthread_barrier_wait(&start_barrier);
#endif

  // if (cinfo.used == 0) return;


  MOVE_STACK_ONTO_SCRATCHPAD();


#if defined _VEC
  if (used){
    tril_gemm_vec(mask, aT, b, c, m, n, t, m_start, m_end, n_start, n_end, vtid_x, vtid_y, vtid, ptid);
  }
#else
  if (used){
    gemm_manycore(aT, b, c, m, n, t, m_start, n_start, ptid, pdim_x, pdim_y);
  }
#endif
  RECOVER_DRAM_STACK();
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *aT, DTYPE *b, DTYPE *c, int m, int n, int t,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->aT = aT;
  args->b = b;
  args->c = c;
  args->m = m;
  args->n = n;
  args->t = t;
  args->tid_x = tid_x;
  args->tid_y = tid_y;
  args->dim_x = dim_x;
  args->dim_y = dim_y;

  return args;
}

void *pthread_kernel(void *args)
{
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early

  pthread_barrier_wait(&start_barrier);

  // call the spmd kernel
  Kern_Args *a = (Kern_Args *)args;

  kernel(a->a, a->aT, a->b, a->c, a->m, a->n, a->t,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  // reset scratchpad config
  SET_PREFETCH_MASK(0, 0, &start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
