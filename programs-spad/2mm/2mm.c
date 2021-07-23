#include <stdio.h>

#include "pthread_launch.h"
#include "2mm.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "util.h"

#include "gemm.h"
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

void __attribute__((optimize("-fno-inline")))
kernel_2mm(int used, int mask, DTYPE *a, DTYPE *b, DTYPE *c, DTYPE *cT, DTYPE *d, DTYPE *e, int m, int n, int t1, int t2,
int m_start, int m_end, int n_start, int n_end, int ptid, int pdim_x, int pdim_y, int vtid_x, int vtid_y, int vtid, int unique_id, int vdim_x){

  #ifdef _VEC
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  if (used) tril_gemm_vec(mask, a, b, c, m, t2, t1, m_start, m_end, n_start, n_end, vtid_x, vtid_y, vtid, ptid);
  
  pthread_barrier_wait(&start_barrier);

  //do transpose at ptid=0
  // if(ptid==0)transpose(c,m,t2,cT);
  transpose_manycore(c,m,t2,cT,ptid,pdim_y*pdim_x);

  WORK_DIV(m,n)
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
  if (used) tril_gemm_vec(mask, cT, d, e, m, n, t2, m_start, m_end, n_start, n_end, vtid_x, vtid_y, vtid, ptid);

  #elif defined MANYCORE_PREFETCH
    SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
    gemm_manycore(a, b, c, m, t2, t1, m_start, n_start, ptid, pdim_x, pdim_y);

    pthread_barrier_wait(&start_barrier);

    //do transpose at ptid=0
    // if(ptid==0)transpose(c,m,t2,cT);

    transpose_manycore(c,m,t2,cT,ptid,pdim_y*pdim_x);

    SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
    VECTOR_EPOCH(mask);
    gemm_manycore(cT, d, e, m, n, t2, m_start, n_start, ptid, pdim_x, pdim_y);


  #else
  // need to wait after tranpose!
  pthread_barrier_wait(&start_barrier);
  gemm_manycore(a, b, c, m, t2, t1, m_start, n_start, ptid, pdim_x, pdim_y);
  pthread_barrier_wait(&start_barrier);
  //do transpose
  // if(ptid==0)transpose(c,m,t2,cT);
  transpose_manycore(c,m,t2,cT,ptid,pdim_y*pdim_x);
  pthread_barrier_wait(&start_barrier);
  gemm_manycore(cT, d, e, m, n, t2, m_start, n_start, ptid, pdim_x, pdim_y);
  #endif

  pthread_barrier_wait(&start_barrier);

}

void kernel(DTYPE *a, DTYPE *aT, DTYPE *b, DTYPE *c, DTYPE *cT, DTYPE *d, DTYPE *e, int m, int n, int t1, int t2,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y)
{

  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0) {
    stats_on();
  }

  
  int m_start = 0;
  int m_end = m;
  int n_start = 0;
  int n_end = n;

  
  #ifdef _VEC
  #if VECTOR_LEN==4
  SET_USEFUL_VARIABLES_V4(ptid_x, ptid_y, pdim_x, pdim_y);
  #elif VECTOR_LEN==16
  SET_USEFUL_VARIABLES_V16(ptid_x, ptid_y, pdim_x, pdim_y);
  #endif

  WORK_DIV(m,t2)

  #else
  SET_USEFUL_VARIABLES_MANYCORE(ptid_x, ptid_y, pdim_x, pdim_y);
  //do work division here

  m_start  = ptid_y * BLK_DIM;
  n_start  = ptid_x * BLK_DIM;
  #endif

  transpose_manycore(a,m,t1,aT,ptid,pdim_x*pdim_y);
  a=aT;

  #ifdef PER_CORE_SIMD
  vsetvl_e32m1(HARDWARE_VECTOR_LEN);
  #endif

  // move stack onto scratchpad for faster local access than default on DRAM
  // MOVE_STACK_ONTO_SCRATCHPAD();

  unsigned long long *spTop = getSpTop(ptid);
  // // guess the remaining of the part of the frame (n) that might be needed?? here n = 30
  spTop -= 100;

  unsigned long long stackLoc;
  unsigned long long temp;
  #pragma GCC unroll(100)
  for(int i=0;i<100;i++){
    asm volatile("ld t0, %[id](sp)\n\t"
                "sd t0, %[id](%[spad])\n\t"
                : "=r"(temp)
                : [id] "i"(i*8), [spad] "r"(spTop));
  }
  asm volatile (                                      
      "addi t0, sp, 0\n\t"                            
      "addi sp, %[spad], 0\n\t"                       
      "addi %[dest], t0, 0\n\t"                       
      : [ dest ] "=r"(stackLoc)                       
      : [ spad ] "r"(spTop));

  kernel_2mm(used, mask, a, b,c,cT,d,e,m,n,t1,t2,
            m_start, m_end, n_start, n_end, ptid, pdim_x, pdim_y, vtid_x, vtid_y, vtid, unique_id, vdim_x);

  // restore stack pointer to DRAM
  RECOVER_DRAM_STACK();
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *aT, DTYPE *b, DTYPE *c, DTYPE *cT, DTYPE *d, DTYPE *e, int m, int n, int t1, int t2,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->aT = aT;
  args->b = b;
  args->c = c;
  args->cT = cT;
  args->d = d;
  args->e = e;
  args->m = m;
  args->n = n;
  args->t1 = t1;
  args->t2 = t2;
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

  kernel(a->a, a->aT, a->b, a->c, a->cT, a->d, a->e, a->m, a->n, a->t1, a->t2,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  // reset scratchpad config
  SET_PREFETCH_MASK(0, 0, &start_barrier);
  
  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
