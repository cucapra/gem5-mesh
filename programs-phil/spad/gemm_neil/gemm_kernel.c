#include "gemm_kernel.h"

#define BLK_DIM 4

#define REGION_SIZE BLK_DIM
#define NUM_REGIONS (512 / REGION_SIZE)

// #define VECTOR_CORE
// #define SCALAR_CORE

static inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

void gemm_vec_simd(int mask, DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
                   int m_start, int m_end, int n_start, int n_end, int tid_x, int tid_y, int tid, int ptid)
{

  VECTOR_EPOCH(mask);

#ifdef SCALAR_CORE
  int spadRegion = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);

  int dim_x = 2; //num cpu in a group in x dim
  int dim_y = 2;
  int total_cores = dim_x * dim_y;

  int offset_x, offset_y;
  offset_x = BLK_DIM * dim_x;
  offset_y = BLK_DIM * dim_y;

  DTYPE *sp_a;
  DTYPE *sp_b;
  ISSUE_VINST(fable0);

  //assuming m_start-m_end is divisble by BLK_DIM
  for (int i0 = m_start; i0 < m_end; i0 += offset_y)
  {
    // int i_st = i0 + (tid_y * BLK_DIM);
    ISSUE_VINST(hoist1);
    for (int j0 = n_start; j0 < n_end; j0 += offset_x)
    {
      // int j_st = j0 + (tid_x * BLK_DIM);
      ISSUE_VINST(hoist2);
      for (int k = 0; k < t; k++)
      {
        sp_a = spAddr + spadRegion * REGION_SIZE + 0;
        sp_b = spAddr + spadRegion * REGION_SIZE + REGION_SIZE / 2;

        // fetch a in scratchpad
        for (int i = 0; i < (offset_y / total_cores); i++)
        {
          VPREFETCH(sp_a + i, a + _idx_(k, i0 + (i * total_cores), m), 0);
        }

        // fetch b in scratchpad
        for (int j = 0; j < (offset_x / total_cores); j++)
        {
          VPREFETCH(sp_b + j, b + _idx_(k, j0 + (j * total_cores), m), 0);
        }
        ISSUE_VINST(fable123);
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
      }
      ISSUE_VINST(fable4567);
    }
    ISSUE_VINST(fable8);
  }

  ISSUE_VINST(stack_end);
  // devec with unique tag
  DEVEC(devec_0);

#elif defined VECTOR_CORE
  DTYPE *sp_c;
  int i_st, j_st;
  volatile int bh1, bh2, bh3;
  DTYPE a_, b_;
  int spadRegion;
  DTYPE *sp_all[4] = {(DTYPE *)getSpAddr(1, 0), (DTYPE *)getSpAddr(2, 0), (DTYPE *)getSpAddr(5, 0), (DTYPE *)getSpAddr(6, 0)};
  DTYPE *spAddr;
  int offset_x, offset_y;

  int dim_x = 2; //num cpu in a group in x dim
  int dim_y = 2;
  int total_cores = dim_x * dim_y;

  spadRegion = 0;
  spAddr = sp_all[tid];
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
#pragma unroll
        for (int i = 0; i < BLK_DIM; i++)
        {
          for (int j = 0; j < BLK_DIM; j++)
          {
            int which_sp_a = (tid_y * BLK_DIM + i) % total_cores;
            int sp_offset_a = (tid_y * BLK_DIM + i) / total_cores;
            DTYPE *addr_a = sp_all[which_sp_a] + spadRegion * REGION_SIZE + sp_offset_a;

            int which_sp_b = (tid_x * BLK_DIM + j) % total_cores;
            int sp_offset_b = (tid_x * BLK_DIM + j) / total_cores;
            DTYPE *addr_b = sp_all[which_sp_b] + spadRegion * REGION_SIZE + REGION_SIZE / 2 + sp_offset_b;

            LWSPEC(a_, addr_a, 0);
            LWSPEC(b_, addr_b, 0);

            sp_c[_idx_(i, j, BLK_DIM)] += a_ * b_;
          }
        }
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
        REMEM(0); //need to do this collectively for all vector cores if values shared!!
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

  /*
  //vector region

  DTYPE *sp_c;
  int i,j;
  int which_sp_a,sp_offset_a;
  int which_sp_b,sp_offset_b;
  DTYPE a_,b_;

  int i_st, j_st;

  DTYPE *addr_a,*addr_b;

  fable0:
    tid_x = tid%dim_x;
    tid_y = tid/dim_y;
    spadRegion = 0;
    //DTYPE *sp_all[4] = {(DTYPE*)getSpAddr(0,0),(DTYPE*)getSpAddr(1,0),(DTYPE*)getSpAddr(2,0),(DTYPE*)getSpAddr(3,0)};
    sp_c = spAddr + NUM_REGIONS*REGION_SIZE;

    

    i_st = m_start + (tid_y * BLK_DIM);
    j_st = n_start + (tid_x * BLK_DIM);

  fable1:
    i=0;
    j=0;

  fable2:
    which_sp_a = (tid_y*BLK_DIM + i)%total_cores;
    sp_offset_a = (tid_y*BLK_DIM + i)/total_cores;

    addr_a = sp_all[which_sp_a] + spadRegion*REGION_SIZE + sp_offset_a;

    which_sp_b = (tid_x*BLK_DIM + j)%total_cores;
    sp_offset_b = (tid_x*BLK_DIM + j)/total_cores;
    addr_b = sp_all[which_sp_b] + spadRegion*REGION_SIZE + REGION_SIZE/2 + sp_offset_b;
    

    LWSPEC(a_, addr_a, 0); 
    LWSPEC(b_, addr_b, 0);

    sp_c[_idx_(i,j,BLK_DIM)] += a_*b_;
    j++;
    spadRegion = (spadRegion + 1) % NUM_REGIONS;
    REMEM(0); //need to do this collectively for all vector cores if values shared!!

    

  fable3:
    i++;
    j=0;

    // need this jump to create loop carry dependencies, but this should be remove later
    //asm volatile goto("j %l[fable2]\n\t"::::fable2);

  fable4:
    i=0;
    j=0;

  fable5:
    STORE_NOACK(sp_c[_idx_(i,j,BLK_DIM)],c + _idx_(i+i_st, j+j_st, n), 0);
    sp_c[_idx_(i,j,BLK_DIM)]=0;

  fable6:
    j=0;
    i++;

    // need this jump to create loop carry dependencies, but this should be remove later
    //asm volatile goto("j %l[fable5]\n\t"::::fable5);

  fable7:
    j_st++;

  fable8:
    i_st++;
    j_st=0;

    // need this jump to create loop carry dependencies, but this should be remove later
    asm volatile goto("j %l[fable1]\n\t"::::fable1);

  return;

  */
}
