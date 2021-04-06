#include "gemm_kernel.h"

// #define VECTOR_CORE
// #define SCALAR_CORE

#ifdef _VEC

inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}


inline void vert_prefetch(int *sp_a_offset, int *sp_b_offset, int *spadRegion, int dim_y, int dim_x, int k,
                          DTYPE *a, int m_start, int m, DTYPE *b, int n_start, int n){

  *sp_a_offset = *spadRegion * REGION_SIZE;
  *sp_b_offset = *sp_a_offset + REGION_SIZE / 2;

  for (int yy = 0; yy < dim_y; yy++)
  {
    for (int xx = 0; xx < dim_x; xx++){
      //fetch a
      VPREFETCH_L(*sp_a_offset, a + _idx_(k, m_start + yy * BLK_DIM, m), xx + yy * dim_x, BLK_DIM,TO_ONE_CORE);
    
      //fetch b
      VPREFETCH_L(*sp_b_offset, b + _idx_(k, n_start + xx * BLK_DIM, n), xx + yy * dim_x, BLK_DIM,TO_ONE_CORE);
    }
  }
  *spadRegion = (*spadRegion + 1) % NUM_REGIONS;
}

void tril_gemm_vec(int mask, DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
                   int m_start, int m_end, int n_start, int n_end, int vtid_x, int vtid_y, int vtid, int ptid)
{

  

#ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);
  ISSUE_VINST(init); // issue vector block early so that vector cores don't stay idol
  int spadRegion = 0;

#if VECTOR_LEN==4
  int dim_x = 2; //num cpu in a group in x dim
  int dim_y = 2;

#elif VECTOR_LEN==16
  int dim_x = 4; //num cpu in a group in x dim
  int dim_y = 4;
#endif

  int offset_x, offset_y;
  offset_x = BLK_DIM * dim_x;
  offset_y = BLK_DIM * dim_y;

  int sp_a_offset, sp_b_offset;

  int vector_iter=0;
  int level3_size = t;
  int level2_size = (n_end-n_start)/offset_x;

  /* ------------ prefetch ahead of issuing ----------*/
  int iter_ahead = INIT_FRAMES;
  for (int k = 0; k < iter_ahead; k++){
    vert_prefetch(&sp_a_offset, &sp_b_offset, &spadRegion, dim_y, dim_x, k, 
                  a, m_start, m , b, n_start, n);
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

        // ---------------prefetch region ------------------
        vert_prefetch(&sp_a_offset, &sp_b_offset, &spadRegion, dim_y, dim_x, k, 
                  a, i0, m , b, j0, n);
        // ----------------prefetch end -------------------- 

        ISSUE_VINST(fable123);
        vector_iter++;

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

  ISSUE_VINST(vector_stack);
  // devec with unique tag
  DEVEC(devec_0);

  asm volatile("fence\n\t");

  asm("trillium vissue_delim return scalar_return"); 
  return;

  init:
    asm("trillium glue_point init");
  hoist1:
    asm("trillium glue_point hoist1");
  hoist2:
    asm("trillium glue_point hoist2");
  fable123:
    asm("trillium glue_point fable123");
  fable4567:
    asm("trillium glue_point fable4567");
  fable8:
    asm("trillium glue_point fable8");
  vector_stack:
    asm("trillium glue_point vector_stack");

#elif defined VECTOR_CORE
  asm("trillium vissue_delim until_next init");
  DTYPE *sp_c;
  int i_st, j_st;
  volatile int bh1, bh2, bh3;
  DTYPE a_, b_;
  int spadRegion;

  int *ptid_group = getSpAddr(ptid,NUM_REGIONS * REGION_SIZE + BLK_DIM*BLK_DIM + 10);
  DTYPE *sp_all[VECTOR_LEN];

  #if VECTOR_LEN==4
  int dim_x = 2; //num cpu in a group in x dim
  int dim_y = 2;

  #elif VECTOR_LEN==16

  int dim_x = 4; //num cpu in a group in x dim
  int dim_y = 4;
  #endif

  #ifdef PER_CORE_SIMD
  vsetvl_e32m1(HARDWARE_VECTOR_LEN);
  #endif
  
  DTYPE *spAddr;
  int offset_x, offset_y;
  
  spadRegion = 0;
  // spAddr = sp_all[tid];
  spAddr = (DTYPE *)getSpAddr(ptid, 0);
  sp_c = spAddr + NUM_REGIONS * REGION_SIZE;
  offset_x = BLK_DIM * dim_x;
  offset_y = BLK_DIM * dim_y;

  i_st = m_start + (vtid_y * BLK_DIM);
  do
  {
    asm("trillium vissue_delim until_next hoist1");
    j_st = n_start + (vtid_x * BLK_DIM);
    do
    {
      asm("trillium vissue_delim until_next hoist2");
      do
      {
        #ifdef PER_CORE_SIMD
        vsetvl_e32m1(HARDWARE_VECTOR_LEN);
        #endif

        // asm("trillium vissue_delim until_next fable123");
        asm("trillium vissue_delim if_begin fable123");
        FRAME_START(REGION_SIZE);
        #pragma GCC unroll(16)
        for (int i = 0; i < BLK_DIM; i++)
        {
          #ifdef PER_CORE_SIMD
          DTYPE vaT = spAddr[spadRegion * REGION_SIZE + i]*ALPHA;
          vfloat32m1_t vb  = vle32_v_f32m1(&spAddr[spadRegion * REGION_SIZE + REGION_SIZE / 2]);
          vfloat32m1_t vc  = vle32_v_f32m1(&sp_c[_idx_(i, 0, BLK_DIM)]);
          vfloat32m1_t vcp = vfmul_vf_f32m1(vb,vaT);
          vc = vfadd_vv_f32m1(vc, vcp);
          vse32_v_f32m1(&sp_c[_idx_(i, 0, BLK_DIM)], vc);
          #else
          #pragma GCC unroll(16)
          for (int j = 0; j < BLK_DIM; j++)
          {
            DTYPE *addr_a = spAddr + spadRegion * REGION_SIZE + i;
            DTYPE *addr_b = spAddr + spadRegion * REGION_SIZE + REGION_SIZE / 2 + j;

            a_= *addr_a;
            b_= *addr_b;

            sp_c[_idx_(i, j, BLK_DIM)] += ALPHA* a_ * b_;
          }
          #endif
        }
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
        REMEM(REGION_SIZE); //need to do this collectively for all vector cores if values shared!!
        asm("trillium vissue_delim end at_jump");
      }while (bh3);
    asm("trillium vissue_delim until_next fable4567");
      #ifdef PER_CORE_SIMD
      vsetvl_e32m1(HARDWARE_VECTOR_LEN);
      #endif
      #pragma GCC unroll(16)
      for (int i = 0; i < BLK_DIM; i++)
      {
        #ifdef PER_CORE_SIMD

        vfloat32m1_t vc  = vle32_v_f32m1(&c[_idx_(i + i_st, j_st, n)]);
        vfloat32m1_t vspc = vle32_v_f32m1(&sp_c[_idx_(i, 0, BLK_DIM)]);

        vc = vfmul_vf_f32m1(vc, BETA);
        vc = vfadd_vv_f32m1(vc, vspc);
        vse32_v_f32m1(&c[_idx_(i + i_st, j_st, n)], vc);  

        vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
        vse32_v_f32m1(&sp_c[_idx_(i, 0, BLK_DIM)], vzero);

        #else
        #pragma GCC unroll(16)
        for (int j = 0; j < BLK_DIM; j++)
        {
          DTYPE temp = c[_idx_(i+i_st, j+j_st, n)]*BETA;
          temp += sp_c[_idx_(i, j, BLK_DIM)];
          STORE_NOACK(temp, c + _idx_(i + i_st, j + j_st, n), 0);
          sp_c[_idx_(i, j, BLK_DIM)] = 0;
        }
        #endif
      }
      j_st += offset_x;
    }while (bh2);
    asm("trillium vissue_delim until_next fable8");
    i_st += offset_y;
  }while (bh1);

  // mark vector stack cleanup assembly
  asm("trillium vissue_delim return vector_stack");
  return;

#endif
}

#endif