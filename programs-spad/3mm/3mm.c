#include <stdio.h>
#include <stdlib.h>

#include "pthread_launch.h"
#include "3mm.h"
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

}


void kernel(DTYPE *a, DTYPE *aT, DTYPE *b, DTYPE *e, DTYPE *c, DTYPE *cT, DTYPE *d, DTYPE *f, DTYPE *eT, DTYPE *g, 
            int m, int t1, int k, int t2, int n, int ptid_x, int ptid_y, int pdim_x, int pdim_y)
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
  #else
  SET_USEFUL_VARIABLES_MANYCORE(ptid_x, ptid_y, pdim_x, pdim_y);

  //do work division here
  m_start  = ptid_y * BLK_DIM;
  n_start  = ptid_x * BLK_DIM;

  #endif

  transpose_manycore(a,m,t1,aT,ptid,pdim);
  a=aT;
  transpose_manycore(c,k,t2,cT,ptid,pdim);
  c=cT;


  // move stack onto scratchpad for faster local access than default on DRAM
  // MOVE_STACK_ONTO_SCRATCHPAD();

  unsigned long long *spTop = getSpTop(ptid);
  // // guess the remaining of the part of the frame (n) that might be needed?? here n = 30
  spTop -= 110;

  unsigned long long stackLoc;
  unsigned long long temp;
  #pragma GCC unroll(110)
  for(int i=0;i<110;i++){
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

  
#ifdef _VEC
  int uid_x,uid_y;
  int tg_x,tg_y;
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  // E = A(m,t1).B(t1,k)
  WORK_DIV(m,k)
  if(used) {
    // //do work division here
    // int alignment = BLK_DIM * cinfo.vdim_x; //each group should have elements of multiple of this number
    // m_start = roundUp((cinfo.unique_id + 0) * m / cinfo.total_groups, alignment); 
    // m_end = roundUp((cinfo.unique_id + 1) * m / cinfo.total_groups, alignment);

    tril_gemm_vec(mask, a, b, e, m, k, t1, m_start, m_end, n_start, n_end, 
      vtid_x, vtid_y, vtid, ptid);
  }

  // if(ptid==0)printf("Done 1MM\n");

  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  // F = C(k,t2).D(t2,n)
  WORK_DIV(k,n)
  if(used) {
    //do work division here
    // int alignment = BLK_DIM * cinfo.vdim_x; //each group should have elements of multiple of this number
    // m_start = roundUp((cinfo.unique_id + 0) * k / cinfo.total_groups, alignment); 
    // m_end = roundUp((cinfo.unique_id + 1) * k / cinfo.total_groups, alignment);
    
    tril_gemm_vec(mask, c, d, f, k, n, t2, m_start, m_end, n_start, n_end, 
      vtid_x, vtid_y, vtid, ptid);
  }
  // if(ptid==0)printf("Done 2MM\n");
  pthread_barrier_wait(&start_barrier);
  //do transpose at ptid=0
  // if(ptid==0)transpose(e,m,k,eT);
  transpose_manycore(e,m,k,eT,ptid,pdim);

  // if(ptid==0)printf("Done tranpose\n");
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  // G = E(m,k).F(k,n)
  WORK_DIV(m,n)
  if(used) {
    //do work division here
    // int alignment = BLK_DIM * cinfo.vdim_x; //each group should have elements of multiple of this number
    // m_start = roundUp((cinfo.unique_id + 0) * m / cinfo.total_groups, alignment); 
    // m_end = roundUp((cinfo.unique_id + 1) * m / cinfo.total_groups, alignment);
    
    tril_gemm_vec(mask, eT, f, g, m, n, k, m_start, m_end, n_start, n_end, 
      vtid_x, vtid_y, vtid, ptid);
  }
  // if(ptid==0)printf("Done 3MM\n");
#elif defined MANYCORE_PREFETCH

  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
  // E = A(m,t1).B(t1,k)
  gemm_manycore(a, b, e, m, k, t1, m_start, n_start, ptid, pdim_x, pdim_y);
  

  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
  // F = C(k,t2).D(t2,n)
  gemm_manycore(c, d, f, k, n, t2, m_start, n_start, ptid, pdim_x, pdim_y);

  pthread_barrier_wait(&start_barrier);
  //do transpose at ptid=0
  // if(ptid==0)transpose(e,m,k,eT);
  transpose_manycore(e,m,k,eT,ptid,pdim);

  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
  // G = E(m,k).F(k,n)
  gemm_manycore(eT, f, g, m, n, k, m_start, n_start, ptid, pdim_x, pdim_y);


#else
  // E = A(m,t1).B(t1,k)
  gemm_manycore(a, b, e, m, k, t1, m_start, n_start, ptid, pdim_x, pdim_y);

  pthread_barrier_wait(&start_barrier);

  // F = C(k,t2).D(t2,n)
  gemm_manycore(c, d, f, k, n, t2, m_start, n_start, ptid, pdim_x, pdim_y);

  pthread_barrier_wait(&start_barrier);
  // if(ptid==0)transpose(e,m,k,eT);
  transpose_manycore(e,m,k,eT,ptid,pdim);
  
  pthread_barrier_wait(&start_barrier);
  gemm_manycore(eT, f, g, m, n, k, m_start, n_start, ptid, pdim_x, pdim_y);

#endif

  // restore stack pointer to DRAM
  RECOVER_DRAM_STACK();
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *aT, DTYPE *b, DTYPE *e, DTYPE *c, DTYPE *cT, DTYPE *d, DTYPE *f, DTYPE *eT, DTYPE *g, 
                          int m, int t1, int k, int t2, int n, int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->aT = aT;
  args->b = b;
  args->e = e;
  args->c = c;
  args->cT = cT;
  args->d = d;
  args->f = f;
  args->eT = eT;
  args->g = g;
  args->m = m;
  args->t1 = t1;
  args->k = k;
  args->t2 = t2;
  args->n = n;
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

  kernel(a->a, a->aT, a->b, a->e, a->c, a->cT, a->d, a->f, a->eT, a->g, a->m, a->t1, a->k, a->t2, a->n,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);
  
  // reset scratchpad config
  SET_PREFETCH_MASK(0, 0, &start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
