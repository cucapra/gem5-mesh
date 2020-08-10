#include "gesummv_kernel.h"

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

  for (int d = 0; d < VEC_LEN; d++){
    VPREFETCH_L(sp_a_offset, a + _idx_(i+d,j,n), d, REGION_SIZE/3,1); //load A
    VPREFETCH_L(sp_b_offset, b + _idx_(i+d,j,n), d, REGION_SIZE/3,1); //load A
    VPREFETCH_L(sp_x_offset, x + j, d, REGION_SIZE/3,1); //load x
  }

  // sp_a_offset += REGION_SIZE;
  // if (sp_a_offset == NUM_REGIONS*REGION_SIZE) sp_a_offset=0;
  *spadRegion = (*spadRegion + 1) % NUM_REGIONS;
}

void tril_gesummv_vec(int mask, DTYPE *a, DTYPE *b, DTYPE *x, DTYPE *tmp, DTYPE *y, int n, int start, int end, int ptid, int vtid)
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

  for (int i = start; i < end; i+=VEC_LEN) {
    ISSUE_VINST(hoist1_label);
    
    for(int j=0; j<n; j+=REGION_SIZE/3){
      prefetch_gesummv_frame(a, b, x, i, j, n, &spadRegion);

      ISSUE_VINST(dotprod_label);
    }

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

  volatile int bh1,bh2,bh3;
  
  int spadRegion =0;
  // int sp_offset = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);

  int row_thread=start+vtid;
  int col_thread=0;
  DTYPE temp1, temp2;
  do {
    // hoist1:
    asm("trillium vissue_delim until_next hoist1");
    temp1=tmp[row_thread];
    temp2=y[row_thread];
    do {
      // dotprod:
      asm("trillium vissue_delim until_next dotprod");
      // asm("trillium vissue_delim if_begin dotprod");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(8)
      for(int jj=0; jj<REGION_SIZE/3; jj++){
        DTYPE *a_on_sp = spAddr + spadRegion*REGION_SIZE + jj;
        // DTYPE *a_on_sp = spAddr + sp_offset + jj;
        DTYPE *b_on_sp = a_on_sp + REGION_SIZE/3;
        DTYPE *x_on_sp = b_on_sp + REGION_SIZE/3;

        temp1 += (*a_on_sp) * (*x_on_sp);
        temp2 += (*b_on_sp) * (*x_on_sp);
      }
      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      REMEM(REGION_SIZE);
      // sp_offset += REGION_SIZE;
      // if (sp_offset == NUM_REGIONS*REGION_SIZE) sp_offset=0;
      // asm("trillium vissue_delim end at_jump");
    } while(bh2);
    // store_dp:
    asm("trillium vissue_delim until_next store_dp");
    STORE_NOACK(temp1, tmp + row_thread, 0);
    // tmp[row_thread]=temp1;
    STORE_NOACK(ALPHA*temp1 + BETA*temp2, y + row_thread, 0);
    // y[row_thread]= ALPHA*temp1 + BETA*temp2;
    row_thread+=VEC_LEN;
  }while(bh1);

  asm("trillium vissue_delim return vector_stack"); //return delimiter
  return;
  #endif

}

#endif
