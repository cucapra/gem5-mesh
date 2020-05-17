#include "gemm_kernel.h"

#define BLK_DIM 4

// #define SIMD_PRIVATE_16
// #define SIMD_SHARING_16

#ifdef SIMD_PRIVATE_16
#define VERT_LOADS
#define VEC_LEN_16
#define DIM_X 4
#endif

#ifdef SIMD_PRIVATE_4
#define VERT_LOADS
#define VEC_LEN_4
#define DIM_X 2
#endif

#ifdef SIMD_SHARING_16
#define SHARING
#define VEC_LEN_16
#define DIM_X 4
#endif

#ifdef SIMD_SHARING_4
#define SHARING
#define VEC_LEN_4
#define DIM_X 2
#endif

#ifdef SHARING
#define REGION_SIZE (BLK_DIM * 2)/DIM_X
#define NUM_REGIONS (512 / REGION_SIZE)
#else
#define REGION_SIZE (BLK_DIM * 2)
#define NUM_REGIONS (512 / REGION_SIZE)
#endif

#define MIN(a,b) (((a)<(b))?(a):(b))

// #define VECTOR_CORE
// #define SCALAR_CORE

static inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

void gemm_vec_simd(int mask, DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
                   int m_start, int m_end, int n_start, int n_end, int tid_x, int tid_y, int tid, int ptid, int* ptid_group)
{

  VECTOR_EPOCH(mask);

#ifdef SCALAR_CORE
  ISSUE_VINST(fable0); // issue vector block early so that vector cores don't stay idol
  int spadRegion = 0;

#ifdef VEC_LEN_4
  int dim_x = 2; //num cpu in a group in x dim
  int dim_y = 2;

#elif defined VEC_LEN_16
  int dim_x = 4; //num cpu in a group in x dim
  int dim_y = 4;
#endif

  int total_cores = dim_x * dim_y;
  int offset_x, offset_y;
  offset_x = BLK_DIM * dim_x;
  offset_y = BLK_DIM * dim_y;

  int sp_a_offset, sp_b_offset;

  int vector_iter=0;
  int level3_size = t;
  int level2_size = (n_end-n_start)/offset_x;

  /* ------------ prefetch ahead of issuing ----------*/
  int iter_ahead = MIN(t,1);
  for (int k = 0; k < iter_ahead; k++){
    sp_a_offset = spadRegion * REGION_SIZE;
    sp_b_offset = sp_a_offset + REGION_SIZE / 2;

  // fetch a in scratchpad
  #ifdef SHARING
    for (int i = 0; i < (offset_y / total_cores); i++)
    {
      // VPREFETCH(sp_a + i, a + _idx_(k, m_start + (i * total_cores), m), 0);
      VPREFETCH_L(sp_a_offset + i, a + _idx_(k, m_start + (i * total_cores), m), 0, total_cores,0);
      VPREFETCH_R(sp_a_offset + i, a + _idx_(k, m_start + (i * total_cores), m), 0, total_cores,0);
    }

    // fetch b in scratchpad
    for (int j = 0; j < (offset_x / total_cores); j++)
    {
      // VPREFETCH(sp_b + j, b + _idx_(k, n_start + (j * total_cores), m), 0);
      VPREFETCH_L(sp_b_offset + j, b + _idx_(k, n_start + (j * total_cores), n), 0, total_cores,0);
      VPREFETCH_R(sp_b_offset + j, b + _idx_(k, n_start + (j * total_cores), n), 0, total_cores,0);
    }
  #else

    #ifdef VERT_LOADS
    for (int yy = 0; yy < dim_y; yy++)
    {
      for (int xx = 0; xx < dim_x; xx++)
      {
        //fetch a
        VPREFETCH_L(sp_a_offset, a + _idx_(k, m_start + yy * BLK_DIM, m), xx + yy * dim_x, BLK_DIM,1);
        VPREFETCH_R(sp_a_offset, a + _idx_(k, m_start + yy * BLK_DIM, m), xx + yy * dim_x, BLK_DIM,1);

        //fetch b
        VPREFETCH_L(sp_b_offset, b + _idx_(k, n_start + xx * BLK_DIM, n), xx + yy * dim_x, BLK_DIM,1);
        VPREFETCH_R(sp_b_offset, b + _idx_(k, n_start + xx * BLK_DIM, n), xx + yy * dim_x, BLK_DIM,1);
      }
    }

    #else
    //fetch a one element at a time
    for (int yy = 0; yy < dim_y; yy++)
    {
      for (int i = 0; i < BLK_DIM; i++)
      {
        for (int xx = 0; xx < dim_x; xx++)
        {
          VPREFETCH_L(sp_a_offset + i, a + _idx_(k, m_start + i + yy * BLK_DIM, m), xx + yy * dim_x, 1,0);
        }
      }
    }

    //fetch b
    for (int xx = 0; xx < dim_x; xx++)
    {
      for (int j = 0; j < BLK_DIM; j++)
      {
        for (int yy = 0; yy < dim_y; yy++)
        {
          VPREFETCH_L(sp_b_offset + j, b + _idx_(k, n_start + j + xx * BLK_DIM, m), xx + yy * dim_x, 1,0);
        }
      }
    }
    #endif
  #endif
    spadRegion = (spadRegion + 1) % NUM_REGIONS;
  }
  /* ------------ initial prefetch end ----------*/

  /* ----------------vissue and vprefetch ------------------- */
  //assuming m_start-m_end is divisble by BLK_DIM
  for (int i0 = m_start; i0 < m_end; i0 += offset_y)
  {
    for (int j0 = n_start; j0 < n_end; j0 += offset_x)
    {
      int start_k=0;
      if(i0== m_start && j0==n_start) start_k = iter_ahead; // due to prefetch earlier

      for (int k = start_k; k < t; k++)
      {

        //first thing to do is issue vector cores to compute (don't want to starve them) if scalar core is ahead, if not then send after prefetch
        if(vector_iter%(level2_size*level3_size)==0) ISSUE_VINST(hoist1);
        if(vector_iter%level3_size ==0) ISSUE_VINST(hoist2);
        ISSUE_VINST(fable123);
        vector_iter++;

        // ---------------prefetch region ------------------
  
        sp_a_offset = spadRegion * REGION_SIZE;
        sp_b_offset = sp_a_offset + REGION_SIZE / 2;

// fetch a in scratchpad
#ifdef SHARING
        for (int i = 0; i < (offset_y / total_cores); i++)
        {
          // VPREFETCH(sp_a + i, a + _idx_(k, i0 + (i * total_cores), m), 0);
          VPREFETCH_L(sp_a_offset + i, a + _idx_(k, i0 + (i * total_cores), m), 0, total_cores,0);
          VPREFETCH_R(sp_a_offset + i, a + _idx_(k, i0 + (i * total_cores), m), 0, total_cores,0);
        }

        // fetch b in scratchpad
        for (int j = 0; j < (offset_x / total_cores); j++)
        {
          // VPREFETCH(sp_b + j, b + _idx_(k, j0 + (j * total_cores), m), 0);
          VPREFETCH_L(sp_b_offset + j, b + _idx_(k, j0 + (j * total_cores), n), 0, total_cores,0);
          VPREFETCH_R(sp_b_offset + j, b + _idx_(k, j0 + (j * total_cores), n), 0, total_cores,0);
        }
#else
        #ifdef VERT_LOADS
        for (int yy = 0; yy < dim_y; yy++)
        {
          for (int xx = 0; xx < dim_x; xx++)
          {
            //fetch a
            VPREFETCH_L(sp_a_offset, a + _idx_(k, i0 + yy * BLK_DIM, m), xx + yy * dim_x, BLK_DIM,1);
            VPREFETCH_R(sp_a_offset, a + _idx_(k, i0 + yy * BLK_DIM, m), xx + yy * dim_x, BLK_DIM,1);

            //fetch b
            VPREFETCH_L(sp_b_offset, b + _idx_(k, j0 + xx * BLK_DIM, n), xx + yy * dim_x, BLK_DIM,1);
            VPREFETCH_R(sp_b_offset, b + _idx_(k, j0 + xx * BLK_DIM, n), xx + yy * dim_x, BLK_DIM,1);
          }
        }
        
        #else
        //fetch a one element at a time (todo: might need to vectorize the loads)
        for (int yy = 0; yy < dim_y; yy++)
        {
          for (int i = 0; i < BLK_DIM; i++)
          {
            for (int xx = 0; xx < dim_x; xx++)
            {
              VPREFETCH_L(sp_a_offset + i, a + _idx_(k, i0 + i + yy * BLK_DIM, m), xx + yy * dim_x, 1,0);
            }
          }
        }

        //fetch b
        for (int xx = 0; xx < dim_x; xx++)
        {
          for (int j = 0; j < BLK_DIM; j++)
          {
            for (int yy = 0; yy < dim_y; yy++)
            {
              VPREFETCH_L(sp_b_offset + j, b + _idx_(k, j0 + j + xx * BLK_DIM, m), xx + yy * dim_x, 1,0);
            }
          }
        }
        #endif
#endif
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
        // ----------------prefetch end --------------------

        if(vector_iter%level3_size ==0) ISSUE_VINST(fable4567);
        if(vector_iter%(level2_size*level3_size)==0) ISSUE_VINST(fable8);
        
      }
    }
  }
  /* ------------------ end of vprefetches ----------------------*/

  /* ------------ bunch of vissues to finish off ---------------*/
  for (int k = 0; k < iter_ahead; k++){
    ISSUE_VINST(fable123);
    vector_iter++;
    if(vector_iter%level3_size ==0) ISSUE_VINST(fable4567);
  }
  /* ------------------finish -----------------------------------*/

  ISSUE_VINST(stack_end);
  // devec with unique tag
  DEVEC(devec_0);

#elif defined VECTOR_CORE
  DTYPE *sp_c;
  int i_st, j_st;
  volatile int bh1, bh2, bh3;
  DTYPE a_, b_;
  int spadRegion;
  
  #ifdef VEC_LEN_4

  #ifdef SHARING
  DTYPE *sp_all[4];
  #pragma GCC unroll (4)
  for(int i=0;i<4;i++){
    sp_all[i] = (DTYPE *)getSpAddr(ptid_group[i], 0);
  }
  #endif

  int dim_x = 2; //num cpu in a group in x dim
  int dim_y = 2;

  #elif defined VEC_LEN_16

  #ifdef SHARING
  DTYPE *sp_all[16];
  #pragma GCC unroll (16)
  for(int i=0;i<16;i++){
    sp_all[i] = (DTYPE *)getSpAddr(ptid_group[i], 0);
  }
  #endif

  int dim_x = 4; //num cpu in a group in x dim
  int dim_y = 4;
  
  #endif
  
  int total_cores = dim_x * dim_y;
  DTYPE *spAddr;
  int offset_x, offset_y;
  
  spadRegion = 0;
  // spAddr = sp_all[tid];
  spAddr = (DTYPE *)getSpAddr(ptid, 0);
  sp_c = spAddr + NUM_REGIONS * REGION_SIZE;
  offset_x = BLK_DIM * dim_x;
  offset_y = BLK_DIM * dim_y;

  i_st = m_start + (tid_y * BLK_DIM);
  while (bh1)
  {
    asm("nop");
    j_st = n_start + (tid_x * BLK_DIM);
    while (bh2)
    {
      asm("nop");
      while (bh3)
      {
      fable123:
        FRAME_START(REGION_SIZE);
#pragma unroll
        for (int i = 0; i < BLK_DIM; i++)
        {
          for (int j = 0; j < BLK_DIM; j++)
          {
#ifdef SHARING
            int which_sp_a = (tid_y * BLK_DIM + i) % total_cores;
            int sp_offset_a = (tid_y * BLK_DIM + i) / total_cores;
            DTYPE *addr_a = sp_all[which_sp_a] + spadRegion * REGION_SIZE + sp_offset_a;

            int which_sp_b = (tid_x * BLK_DIM + j) % total_cores;
            int sp_offset_b = (tid_x * BLK_DIM + j) / total_cores;
            DTYPE *addr_b = sp_all[which_sp_b] + spadRegion * REGION_SIZE + REGION_SIZE / 2 + sp_offset_b;
#else
            DTYPE *addr_a = spAddr + spadRegion * REGION_SIZE + i;
            DTYPE *addr_b = spAddr + spadRegion * REGION_SIZE + REGION_SIZE / 2 + j;
#endif
            // LWSPEC(a_, addr_a, 0);
            // LWSPEC(b_, addr_b, 0);

            a_= *addr_a;
            b_= *addr_b;

            sp_c[_idx_(i, j, BLK_DIM)] += a_ * b_;
          }
        }
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
        REMEM(REGION_SIZE); //need to do this collectively for all vector cores if values shared!!
      }
    fable456:
#pragma unroll
      for (int i = 0; i < BLK_DIM; i++)
      {
        for (int j = 0; j < BLK_DIM; j++)
        {
          STORE_NOACK(sp_c[_idx_(i, j, BLK_DIM)], c + _idx_(i + i_st, j + j_st, n), 0);
          sp_c[_idx_(i, j, BLK_DIM)] = 0;
        }
      }
      j_st += offset_x;
    }
    i_st += offset_y;
  }

#endif

  asm volatile("fence\n\t");

  return;

#ifdef SCALAR_CORE
fable0:
  asm("nop");
hoist1:
  asm("nop");
hoist2:
  asm("nop");
fable123:
  asm("nop");
fable4567:
  asm("nop");
fable8:
  asm("nop");
stack_end:
  asm("nop");

  return;

#endif
}