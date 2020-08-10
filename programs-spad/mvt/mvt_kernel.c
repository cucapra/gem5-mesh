#include "mvt_kernel.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

#ifdef _VEC

inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

inline void prefetch_mvt_frame_y1(DTYPE *a, DTYPE *y1, int i, int j, int n, int *spadRegion) {
  int sp_a_offset = *spadRegion * REGION_SIZE;
  int sp_y1_offset = sp_a_offset + REGION_SIZE/2;

  for (int d = 0; d < VEC_LEN; d++){
    VPREFETCH_L(sp_a_offset, a + _idx_(i+d,j,n), d, REGION_SIZE/2,1); //load A, hopefully cache alligned so no vprefetch_R
    VPREFETCH_L(sp_y1_offset, y1 + j, d, REGION_SIZE/2,1); //load x
  }

  *spadRegion = (*spadRegion + 1) % NUM_REGIONS;
}

inline void prefetch_mvt_frame_x1(DTYPE *x1, int i, int *spadRegion) {
  int sp_x1_offset = *spadRegion * REGION_SIZE;
  for (int d = 0; d < REGION_SIZE; d++){
      VPREFETCH_L(sp_x1_offset+d, x1+i, 0, VEC_LEN ,0); //issue same request to fill region
  }
  *spadRegion = (*spadRegion + 1) % NUM_REGIONS;
}

inline void prefetch_mvt_frame_y2(DTYPE *a, DTYPE *y2, int i, int j, int n, int *spadRegion) {
  int sp_a_offset = *spadRegion * REGION_SIZE;
  int sp_y2_offset = sp_a_offset + REGION_SIZE/2;

  for(int ii=0; ii<REGION_SIZE/2; ii++){
    VPREFETCH_L(sp_a_offset+ii, a+_idx_(j+ii,i,n), 0, VEC_LEN ,0);
  }
  for (int d = 0; d < VEC_LEN; d++){
    VPREFETCH_L(sp_y2_offset, y2+j, d, REGION_SIZE/2 ,1); 
  }

  *spadRegion = (*spadRegion + 1) % NUM_REGIONS;
}

inline void prefetch_mvt_frame_x2(DTYPE *x2, int i, int *spadRegion) {
  int sp_x2_offset = *spadRegion * REGION_SIZE;
  for (int d = 0; d < REGION_SIZE; d++){
      VPREFETCH_L(sp_x2_offset+d, x2+i, 0, VEC_LEN ,0);
  }
  *spadRegion = (*spadRegion + 1) % NUM_REGIONS;
}

void tril_mvt_vec(int mask, DTYPE *a, DTYPE *y1, DTYPE *y2, DTYPE *x1, DTYPE *x2, int n, 
                  int start, int end, int ptid, int vtid)
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
  int sp_a_offset, sp_y1_offset, sp_x1_offset, sp_y2_offset, sp_x2_offset;

  int* ptid_group_sp = getSpAddr(ptid,NUM_REGIONS*REGION_SIZE);
  // if(ptid==0)printf("ptid %d %d %d %d\n",ptid_group_sp[0],ptid_group_sp[1],ptid_group_sp[2],ptid_group_sp[3]);

  DTYPE temp;
  for (int i = start; i < end; i+=VEC_LEN) {
    temp=0;
    ISSUE_VINST(hoist1_label);
    
    for(int j=0; j<n; j+=REGION_SIZE/2){
      prefetch_mvt_frame_y1(a, y1, i, j, n, &spadRegion);
      ISSUE_VINST(dotprod_label);
    }

    // ----- prefetch x1[i+vtid] -----
    prefetch_mvt_frame_x1(x1, i, &spadRegion);

    ISSUE_VINST(store_dp_label);

    for(int j=0; j<n; j+=REGION_SIZE/2){
      prefetch_mvt_frame_y2(a, y2, i, j, n, &spadRegion);
      ISSUE_VINST(transpose_dp_label);
    }
    // ----- prefetch x2[i+vtid] -----
    prefetch_mvt_frame_x2(x2, i, &spadRegion);

    ISSUE_VINST(loop_end_label);
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
  transpose_dp_label:
    asm("trillium glue_point transpose_dp");
  loop_end_label:
    asm("trillium glue_point loop_end");
  vector_stack_label: 
    asm("trillium glue_point vector_stack"); //name over here "vector_stack" matches with delimiter in vector code

  #elif defined VECTOR_CORE
  asm("trillium vissue_delim until_next init"); //until_next delimiter used, name (init) over here same as in glue point above
  //vector core code

  volatile int bh1,bh2,bh3;
  
  int spadRegion =0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);

  int row_thread=start+vtid;
  int col_thread=0;
  DTYPE temp;
  do {
    // hoist1:
    asm("trillium vissue_delim until_next hoist1");
    temp=0;
    do {
      // dotprod:
      asm("trillium vissue_delim until_next dotprod");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(8)
      for(int jj=0; jj<REGION_SIZE/2; jj++){
        DTYPE *a_on_sp = spAddr + spadRegion*REGION_SIZE + jj;
        DTYPE *y1_on_sp = a_on_sp + REGION_SIZE/2;

        temp += (*a_on_sp) * (*y1_on_sp);
      }
      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      REMEM(REGION_SIZE);
    } while(bh2);
    // store_dp:
    asm("trillium vissue_delim until_next store_dp");
    x1[row_thread]+=temp;

    FRAME_START(REGION_SIZE);
    temp += *(spAddr + spadRegion*REGION_SIZE);
    REMEM(REGION_SIZE);
    STORE_NOACK(temp, x1 + row_thread, 0);
    spadRegion = (spadRegion + 1) % NUM_REGIONS;
    

    col_thread=0;
    temp=0;
    do {
      
      // transpose_dp:
      asm("trillium vissue_delim until_next transpose_dp");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(8)
      for(int jj=0; jj<REGION_SIZE/2; jj++){
        DTYPE *a_on_sp = spAddr + spadRegion*REGION_SIZE + jj;
        DTYPE *y2_on_sp = a_on_sp + REGION_SIZE/2;

        temp+= *a_on_sp * (*y2_on_sp);
      }
      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      REMEM(REGION_SIZE);
      col_thread+=REGION_SIZE/2;
    } while(bh3);

    asm("trillium vissue_delim until_next loop_end");
    FRAME_START(REGION_SIZE);
    temp += *(spAddr + spadRegion*REGION_SIZE);
    REMEM(REGION_SIZE);
    STORE_NOACK(temp, x2 + row_thread, 0);
    spadRegion = (spadRegion + 1) % NUM_REGIONS;
    
    // x2[row_thread]+=temp;

    row_thread+=VEC_LEN;
    asm volatile("fence\n\t");
  } while(bh1);


  asm("trillium vissue_delim return vector_stack"); //return delimiter
  return;
  #endif

}

#endif