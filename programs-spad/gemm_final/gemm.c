#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "gemm.h"
#include "spad.h"
#include "bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"
#include "reduction.h"

#include "gemm_kernel.h"

static inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}




void __attribute__((optimize("-fno-inline")))
gemm_manycore(DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
     int m_start, int n_start, int ptid, int pdim_x, int pdim_y)
{

#if defined(UNBLOCKED_INNER)
  // use inner product unblocked gemm - minimize DRAM writes in this case
  for (int i = m_start; i < m; i++)
  {
    for (int j = n_start; j < n; j++)
    {
      DTYPE c_temp = 0;
      for (int k = 0; k < t; k++)
      {
        c_temp += a[_idx_(i, k, t)] * b[_idx_(k, j, n)];
      }
      c[_idx_(i, j, n)] = c_temp;
    }
  }

#elif defined(UNBLOCKED_OUTER)
  for (int k = 0; k < t; k++)
  {
    for (int i = m_start; i < m_end; i++)
    {
      for (int j = n_start; j < n_end; j++)
      {
        c[_idx_(i, j, n)] += a[_idx_(i, k, t)] * b[_idx_(k, j, n)];
      }
    }
  }

#else

  int spadRegion = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);

  DTYPE *sp_c = spAddr;

  int offset_x, offset_y;

  offset_x = BLK_DIM * pdim_x;
  offset_y = BLK_DIM * pdim_y;

  //assuming m_start-m_end is divisble by BLK_DIM
  for (int i0 = m_start; i0 < m; i0 += offset_x)
  {
    for (int j0 = n_start; j0 < n; j0 += offset_y)
    {
      for (int k = 0; k < t; k++)
      {

        for (int i = 0; i < BLK_DIM; i++)
        {
          for (int j = 0; j < BLK_DIM; j++)
          {
            DTYPE a_, b_;

            a_ = a[_idx_(i + i0, k, t)];
            b_ = b[_idx_(k, j + j0, n)];
            sp_c[_idx_(i, j, BLK_DIM)] += ALPHA* a_ * b_;
            // c[_idx_(i + i0, j + j0, n)] += a_ * b_;
          }
        }
      }

      for (int i = 0; i < BLK_DIM; i++)
      {
        for (int j = 0; j < BLK_DIM; j++)
        {
          DTYPE temp = c[_idx_(i + i0, j + j0, n)]*BETA;
          temp += sp_c[_idx_(i, j, BLK_DIM)];
          c[_idx_(i + i0, j + j0, n)] = temp;
          // STORE_NOACK(temp, c + _idx_(i + i0, j + j0, n), 0);
          sp_c[_idx_(i, j, BLK_DIM)] = 0;
        }
      }

    }
  }

#endif
}

// actual kernel
void kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y)
{

  // start recording all stats (all cores)
  // use the last thread, b/c this wakes up last?
  if (ptid_x == 0 && ptid_y == 0)
  {
    stats_on();
  }

  int ptid = ptid_x + ptid_y * pdim_x;
  int pdim = pdim_x * pdim_y;
  int start = 0;
  int end = 0;
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

  if(cinfo.used){
    //do work division here
    int alignment = BLK_DIM * cinfo.vdim_x; //each group should have elements of multiple of this number
    start = roundUp((cinfo.unique_id + 0) * m / cinfo.total_groups, alignment); 
    end = roundUp((cinfo.unique_id + 1) * m / cinfo.total_groups, alignment); 
  }

  
#else
  core_config_info_t cinfo = manycore_template(ptid_x, ptid_y, pdim_x, pdim_y);
  
  //do work division here

  int m_start  = ptid_y * BLK_DIM;
  int n_start  = ptid_x * BLK_DIM;
#endif

// get behavior of each core
  #ifdef _VEC
  int mask = getSIMDMask(&cinfo);
  #else
  int mask = 0;
  #endif


  // region based mask for scratchpad
#ifdef _VEC
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
#endif

  if (cinfo.used == 0) return;


MOVE_STACK_ONTO_SCRATCHPAD();


#if defined _VEC
  tril_gemm_vec(mask, a, b, c, m, n, t, start, end, cinfo.vtid_x, cinfo.vtid_y, cinfo.vtid, ptid);
#else
  gemm_manycore(a, b, c, m, n, t, m_start, n_start, ptid, pdim_x, pdim_y);
#endif

  RECOVER_DRAM_STACK();
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
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

  kernel(a->a, a->b, a->c, a->m, a->n, a->t,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  if (a->tid_x == 1 && a->tid_y == 1)
  {
    stats_off();
  }

  return NULL;
}
