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
gemm_manycore(DTYPE *aT, DTYPE *b, DTYPE *c, int m, int n, int t,
     int m_start, int n_start, int ptid, int pdim_x, int pdim_y)
{
  int spadRegion = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);

  DTYPE *sp_c = spAddr + NUM_REGIONS * REGION_SIZE;

  int offset_x, offset_y;

  offset_x = BLK_DIM * pdim_x;
  offset_y = BLK_DIM * pdim_y;

  int sp_a_offset,sp_b_offset;
  int sp_c_offset[2];

  //assuming m_start-m_end is divisble by BLK_DIM
  for (int i0 = m_start; i0 < m; i0 += offset_x)
  {
    for (int j0 = n_start; j0 < n; j0 += offset_y)
    {
      for (int k = 0; k < t; k++)
      {
        #ifdef MANYCORE_PREFETCH
        sp_a_offset = spadRegion * REGION_SIZE;
        sp_b_offset = sp_a_offset + BLK_DIM;

        // fetch a in scratchpad
        VPREFETCH_L(sp_a_offset, aT + _idx_(k, i0, m), 0, BLK_DIM,1);
        // fetch b in scratchpad
        VPREFETCH_L(sp_b_offset, b + _idx_(k, j0, n), 0, BLK_DIM,1);
        FRAME_START(REGION_SIZE);
        #endif
        for (int i = 0; i < BLK_DIM; i++)
        {
          for (int j = 0; j < BLK_DIM; j++)
          {
            DTYPE a_, b_;
            #ifdef MANYCORE_PREFETCH
            a_ = spAddr[sp_a_offset+i];
            b_ = spAddr[sp_b_offset+j];
            #else
            a_ = aT[_idx_(k,i + i0, m)];
            b_ = b[_idx_(k, j + j0, n)];
            #endif
            sp_c[_idx_(i, j, BLK_DIM)] += ALPHA* a_ * b_;
            // c[_idx_(i + i0, j + j0, n)] += a_ * b_;
          }
        }

        #ifdef MANYCORE_PREFETCH
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
        REMEM(REGION_SIZE);
        #endif
      }

      
      for (int ii = 0; ii < BLK_DIM; ii+=2)
      {
        #ifdef MANYCORE_PREFETCH
        // fetch c in scratchpad
        sp_c_offset[0] = spadRegion * REGION_SIZE;
        sp_c_offset[1] = sp_c_offset[0] + BLK_DIM;
        VPREFETCH_L(sp_c_offset[0], c + _idx_(ii + i0, j0, n), 0, BLK_DIM,1);
        VPREFETCH_L(sp_c_offset[1], c + _idx_(ii+1 + i0, j0, n), 0, BLK_DIM,1);
        FRAME_START(REGION_SIZE);
        #endif
        for (int i=ii; i<ii+2; i++){
          for (int j = 0; j < BLK_DIM; j++)
          {
            DTYPE temp;
            #ifdef MANYCORE_PREFETCH
            temp = spAddr[sp_c_offset[i-ii]+j]*BETA;
            #else
            temp = c[_idx_(i + i0, j + j0, n)]*BETA;
            #endif
            temp += sp_c[_idx_(i, j, BLK_DIM)];
            // c[_idx_(i + i0, j + j0, n)] = temp;
            STORE_NOACK(temp, c + _idx_(i + i0, j + j0, n), 0);
            sp_c[_idx_(i, j, BLK_DIM)] = 0;
          }
        }
        #ifdef MANYCORE_PREFETCH
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
        REMEM(REGION_SIZE);
        #endif
      }

    }
  }
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
  #elif defined MANYCORE_PREFETCH
  int mask = getDebugMask(&cinfo);
  #else
  int mask = 0;
  #endif


  // region based mask for scratchpad
#ifdef _VEC
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
#elif defined MANYCORE_PREFETCH
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
#endif

  if (cinfo.used == 0) return;


MOVE_STACK_ONTO_SCRATCHPAD();


#if defined _VEC
  tril_gemm_vec(mask, a, b, c, m, n, t, start, end, cinfo.vtid_x, cinfo.vtid_y, cinfo.vtid, ptid);
#else
  VECTOR_EPOCH(mask);
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
