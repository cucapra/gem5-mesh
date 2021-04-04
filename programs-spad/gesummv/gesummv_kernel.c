#include "gesummv_kernel.h"
#include "util.h"

#ifdef NESTED_SIMD
#include <riscv_vector.h>
#endif

// #define SCALAR_CORE
// #define VECTOR_CORE

#ifdef _VEC

inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

inline void prefetch_gesummv_frame(DTYPE *a, DTYPE *b, DTYPE *x, int i, int j, int n, int *spadRegion) {
  int sp_a_offset = *spadRegion * REGION_SIZE;
  int sp_b_offset = sp_a_offset + REGION_SIZE/3;
  int sp_x_offset = sp_b_offset + REGION_SIZE/3;

  #ifdef LONGLINES
  VPREFETCH_LR(sp_a_offset, a + _idx_(i,j,n), 0, REGION_SIZE/3,TO_ALL_CORES);
  VPREFETCH_LR(sp_b_offset, b + _idx_(i,j,n), 0, REGION_SIZE/3,TO_ALL_CORES);
  VPREFETCH_LR(sp_x_offset, x + j, 0, REGION_SIZE/3,TO_ALL_CORES);
  #else
  for (int d = 0; d < VEC_LEN; d++){
    VPREFETCH_L(sp_a_offset, a + _idx_(i+d,j,n), d, REGION_SIZE/3,TO_ONE_CORE); //load A
    VPREFETCH_L(sp_b_offset, b + _idx_(i+d,j,n), d, REGION_SIZE/3,TO_ONE_CORE); //load A
    VPREFETCH_L(sp_x_offset, x + j, d, REGION_SIZE/3,TO_ONE_CORE); //load x
  }
  #endif

  // sp_a_offset += REGION_SIZE;
  // if (sp_a_offset == NUM_REGIONS*REGION_SIZE) sp_a_offset=0;
  *spadRegion = (*spadRegion + 1) % NUM_REGIONS;
}

void tril_gesummv_vec(int mask, DTYPE *a, DTYPE *b, DTYPE *x, DTYPE *tmp, DTYPE *y,
    int n, int start, int end, int ptid, int vtid, int ptidMailer, int linkId)
{
  //this template uses separate scalar and vector code blocks but they can be interspersed as well as shown here
  //https://github.com/cucapra/gem5-mesh/wiki/Trilliasm-Language-Overview:-Vector-SIMD-in-C

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  //---------------------------------
  //scalar core code iterspersed with vissue
  ISSUE_VINST(init_label); //eg: this block will deal with initila stack manipulation and initilaization of variables
  //-----------------------------------

   //prefetch variables
  int spadRegion = 0;
  // int sp_a_offset, sp_b_offset, sp_x_offset, sp_y_offset, sp_tmp_offset;
  // sp_a_offset=0;

  int startOffset = min(INIT_FRAMES*J_STRIDE, n);

  #ifdef LONGLINES
  int numCompleted = 0;
  volatile int *sp_ptr = (int*)getSpAddr(ptid, 0);
  #endif
  

  for (int i = start; i < end; i+=I_STRIDE) {
    ISSUE_VINST(hoist1_label);

    for (int j = 0; j < startOffset; j+=J_STRIDE) {
      prefetch_gesummv_frame(a, b, x, i, j, n, &spadRegion);
    }
    
    for(int j=startOffset; j<n; j+=J_STRIDE){
      prefetch_gesummv_frame(a, b, x, i, j, n, &spadRegion);
      ISSUE_VINST(dotprod_label);
    }

    for (int j = n - startOffset; j < n; j+=J_STRIDE) {
      ISSUE_VINST(dotprod_label);
    }

    #ifdef LONGLINES
    SCALAR_SYNC_WITH_REDUCTION(sp_ptr, numCompleted);
    #endif

    ISSUE_VINST(store_dp_label);

  }


  //issue stack end portions of vector cores
  ISSUE_VINST(vector_stack_label);
  // devec with unique tag
  DEVEC(devec_0);

  //fence for all cores to ensure memory operations have completed
  asm volatile("fence\n\t");

  asm("trillium vissue_delim return scalar_return"); //return delimiter, delimiters can be of many types
  return;

  //all the vissue labels below:

  init_label: //this name matches with vissue label name
    asm("trillium glue_point init"); //name over here "init" matches with delimiter in vector code
  hoist1_label:
    asm("trillium glue_point hoist1");
  dotprod_label:
    asm("trillium glue_point dotprod");
  store_dp_label:
    asm("trillium glue_point store_dp");
  vector_stack_label: 
    asm("trillium glue_point vector_stack"); //name over here "vector_stack" matches with delimiter in vector code

  #elif defined VECTOR_CORE
  asm("trillium vissue_delim until_next init"); //until_next delimiter used, name (init) over here same as in glue point above
  //vector core code

  volatile int bh1,bh2;
  
  int spadRegion =0;
  // int sp_offset = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);

  #ifdef NESTED_SIMD
  vsetvl_e32m1(NESTED_SIMD_VLEN);
  vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); 
  #endif

  #ifdef LONGLINES
  int row_thread=start;
  int sp_origin = MAILER_OFFSET + (linkId * PER_CORE_MAILER_FRAME) + vtid;
  DTYPE* sp_origin_ptr = (DTYPE*)getSpAddr(ptidMailer, 0);
  #else
  int row_thread=start+vtid;
  #endif

  DTYPE temp1, temp2;
  do {
    // hoist1:
    asm("trillium vissue_delim until_next hoist1");
    #ifdef LONGLINES
    // TODO need to fetch these in reduction cores
    temp1 = 0;
    temp2 = 0;
    #else
    temp1=tmp[row_thread];
    temp2=y[row_thread];
    #endif

    #ifdef NESTED_SIMD
    size_t l = vsetvl_e32m1(NESTED_SIMD_VLEN);
    vfloat32m1_t vtempa = vzero;
    vfloat32m1_t vtempb = vzero;
    #endif

    do {
      // dotprod:
      asm("trillium vissue_delim until_next dotprod");
      // asm("trillium vissue_delim if_begin dotprod");

      #ifdef NESTED_SIMD
      l = vsetvl_e32m1(NESTED_SIMD_VLEN);
      #endif

      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(16)
      for(int jj=0; jj<REGION_SIZE/3; jj+=NESTED_SIMD_VLEN){
        DTYPE *a_on_sp = spAddr + spadRegion*REGION_SIZE + jj;
        // DTYPE *a_on_sp = spAddr + sp_offset + jj;
        DTYPE *b_on_sp = a_on_sp + REGION_SIZE/3;
        DTYPE *x_on_sp = b_on_sp + REGION_SIZE/3;

        #ifdef NESTED_SIMD
        vfloat32m1_t va = vle32_v_f32m1(a_on_sp);
        vfloat32m1_t vb = vle32_v_f32m1(b_on_sp);
        vfloat32m1_t vx = vle32_v_f32m1(x_on_sp);
        va = vfmul_vv_f32m1(va, vx);
        vb = vfmul_vv_f32m1(vb, vx);
        vtempa = vfadd_vv_f32m1(vtempa, va);
        vtempb = vfadd_vv_f32m1(vtempb, vb);
        #else
        temp1 += (*a_on_sp) * (*x_on_sp);
        temp2 += (*b_on_sp) * (*x_on_sp);
        #endif
      }

      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      REMEM(REGION_SIZE);
      // sp_offset += REGION_SIZE;
      // if (sp_offset == NUM_REGIONS*REGION_SIZE) sp_offset=0;
      // asm("trillium vissue_delim end at_jump");
    } while(bh2);
    // store_dp:
    asm("trillium vissue_delim until_next store_dp");

    #ifdef NESTED_SIMD
    vfloat32m1_t vreda = vfredsum_vs_f32m1_f32m1(vtempa, vtempa, vzero);
    vfloat32m1_t vredb = vfredsum_vs_f32m1_f32m1(vtempb, vtempb, vzero);
    temp1 += vfmv_f_s_f32m1_f32(vreda);
    temp2 += vfmv_f_s_f32m1_f32(vredb);
    #endif

    DTYPE y_i = ALPHA*temp1 + BETA*temp2;

    #ifdef LONGLINES
    FSTORE_NOACK(y_i, &sp_origin_ptr[sp_origin], 0);
    sp_origin+=SUB_FRAME_SIZE;
    sp_origin = sp_origin % MAILER_POST_FRAME_WORD;
    #else
    // FSTORE_NOACK(temp1, tmp + row_thread, 0);
    FSTORE_NOACK(y_i, y + row_thread, 0);
    row_thread+=I_STRIDE;
    #endif
  }while(bh1);

  asm("trillium vissue_delim return vector_stack"); //return delimiter
  return;
  #endif

}

#endif
