#include <stdio.h>

#include "pthread_launch.h"
#include "gemm_common.h"
#include "spad.h"
#include "bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"
#include "reduction.h"
#include "util.h"


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
        #ifdef C_PREFETCH
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
            #ifdef C_PREFETCH
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
        #ifdef C_PREFETCH
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
        REMEM(REGION_SIZE);
        #endif
      }

    }
  }
}