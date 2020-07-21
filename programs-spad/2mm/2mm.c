#include <stdio.h>

#include "pthread_launch.h"
#include "2mm.h"
#include "spad.h"
#include "bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"
#include "reduction.h"
#include "util.h"

#include "gemm.h"
#include "gemm_kernel.h"

void transpose(DTYPE *a, int row, int col, DTYPE *aT){

  for(int i=0; i<row; i++){
    for(int j=i; j<col; j++){
      aT[i*col+j] = a[j*row+i];
      aT[j*row+i] = a[i*col+j];
    }
  }
}

void __attribute__((optimize("-fno-inline")))
kernel_2mm(int used, int mask, DTYPE *a, DTYPE *b, DTYPE *c, DTYPE *cT, DTYPE *d, DTYPE *e, int m, int n, int t1, int t2,
            int m_start, int m_end, int n_start, int n_end, int ptid, int pdim_x, int pdim_y, int vtid_x, int vtid_y, int vtid){

  #ifdef _VEC
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  if(ptid==0) printf("launching gemm kernel\n");
  if (used) tril_gemm_vec(mask, a, b, c, m, t2, t1, m_start, m_end, vtid_x, vtid_y, vtid, ptid);
  
  pthread_barrier_wait(&start_barrier);

  //do transpose at ptid=0
  if(ptid==0)transpose(c,m,t2,cT);

  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
  if (used) tril_gemm_vec(mask, cT, d, e, m, n, t2, m_start, m_end, vtid_x, vtid_y, vtid, ptid);

  #elif defined MANYCORE_PREFETCH
    SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
    VECTOR_EPOCH(mask);
    gemm_manycore(a, b, c, m, t2, t1, m_start, n_start, ptid, pdim_x, pdim_y);

    pthread_barrier_wait(&start_barrier);

    //do transpose at ptid=0
    if(ptid==0)transpose(c,m,t2,cT);

    SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
    VECTOR_EPOCH(mask);
    gemm_manycore(cT, d, e, m, n, t2, m_start, n_start, ptid, pdim_x, pdim_y);


  #else
  gemm_manycore(a, b, c, m, t2, t1, m_start, n_start, ptid, pdim_x, pdim_y);
  pthread_barrier_wait(&start_barrier);
  //do transpose
  if(ptid==0)transpose(c,m,t2);
  pthread_barrier_wait(&start_barrier);
  gemm_manycore(cT, d, e, m, n, t2, m_start, n_start, ptid, pdim_x, pdim_y);
  #endif

}

void kernel(DTYPE *a, DTYPE *b, DTYPE *c, DTYPE *cT, DTYPE *d, DTYPE *e, int m, int n, int t1, int t2,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y)
{

  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0) {
    stats_on();
  }

  // linearize tid and dim
  int ptid = ptid_x + ptid_y * pdim_x;
  int pdim = pdim_x * pdim_y;
  int m_start = 0;
  int m_end = m;
  int n_start = 0;
  int n_end = n;
  int vdim;
  template_info_t tinfo;

  #ifdef _VEC
  #if VEC_LEN==4
  tinfo = init_template_4x4_2x2();
  #elif VEC_LEN==16
  tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);
  vdim = cinfo.vdim_x*cinfo.vdim_y;

  if(cinfo.used) {
    //do work division here
    int alignment = BLK_DIM * cinfo.vdim_x; //each group should have elements of multiple of this number
    m_start = roundUp((cinfo.unique_id + 0) * m / cinfo.total_groups, alignment); 
    m_end = roundUp((cinfo.unique_id + 1) * m / cinfo.total_groups, alignment); 
  }

  #else
  core_config_info_t cinfo = manycore_template(ptid_x, ptid_y, pdim_x, pdim_y);

  //do work division here

  m_start  = ptid_y * BLK_DIM;
  n_start  = ptid_x * BLK_DIM;
  #endif

  // get behavior of each core
  #ifdef _VEC
  int mask = getSIMDMask(&cinfo);
  #elif defined MANYCORE_PREFETCH
  int mask = getDebugMask(&cinfo);
  #else
  int mask = 0;
  #endif

  if(ptid==0) printf("moving stack on spad\n");
  // move stack onto scratchpad for faster local access than default on DRAM
  // MOVE_STACK_ONTO_SCRATCHPAD();

    unsigned long long *spTop = getSpTop(ptid);
  // // guess the remaining of the part of the frame (n) that might be needed?? here n = 30
  spTop -= 40;

  unsigned long long stackLoc;
  unsigned long long temp;
  #pragma GCC unroll(40)
  for(int i=0;i<40;i++){
    asm volatile("ld t0, %[id](sp)\n\t"
                "sd t0, %[id](%[spad])\n\t"
                : "=r"(temp)
                : [id] "i"(i*8), [spad] "r"(spTop));
  }
  if(ptid==0) printf("done copying elements on spad\n");
  asm volatile (// save the stack ptr
      "addi %[dest], sp, 0\n\t"
      // overwrite stack ptr
      "addi sp, %[spad], 0\n\t"
      : [ dest ] "=r"(stackLoc)
      : [ spad ] "r"(spTop));

  if(ptid==0) printf("done moving stack on spad\n");

  kernel_2mm(cinfo.used, mask, a, b,c,cT,d,e,m,n,t1,t2,
            m_start, m_end, n_start, n_end, ptid, pdim_x, pdim_y, cinfo.vtid_x, cinfo.vtid_y, cinfo.vtid);

  // restore stack pointer to DRAM
  RECOVER_DRAM_STACK();
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, DTYPE *c, DTYPE *cT, DTYPE *d, DTYPE *e, int m, int n, int t1, int t2,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
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

  kernel(a->a, a->b, a->c, a->cT, a->d, a->e, a->m, a->n, a->t1, a->t2,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
