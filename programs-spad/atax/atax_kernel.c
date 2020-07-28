#include "atax_kernel.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

static inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

void tril_atax(int mask, DTYPE *a, DTYPE *_x, DTYPE *_y_partial, DTYPE *ax, int nx, int ny,
      int nx_start, int nx_end, int ptid, int vtid, int dim, int* ptid_group)
{

  
  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);
  ISSUE_VINST(init); // issue vector block early so that vector cores don't stay idol
  
  int* ptid_group_sp = getSpAddr(ptid,10);
  // if(ptid==0)printf("ptid %d %d %d %d\n",ptid_group_sp[0],ptid_group_sp[1],ptid_group_sp[2],ptid_group_sp[3]);

  //prefetch variables
  int spadRegion = 0;
  int sp_a_offset, sp_x_offset, sp_ypart_offset;

  DTYPE temp;
  for (int i = nx_start; i < nx_end; i+=dim) {
    temp=0;
    ISSUE_VINST(hoist1);
    for(int j=0; j<ny; j+=PREFETCH_LEN){
      sp_a_offset = spadRegion * REGION_SIZE;
      sp_x_offset = sp_a_offset + REGION_SIZE/2;

      for (int d = 0; d < dim; d++){
        VPREFETCH_L(sp_a_offset, a + _idx_(i+d,j,ny), d, PREFETCH_LEN,1); //load A, hopefully cache alligned so no vprefetch_R
        VPREFETCH_L(sp_x_offset, _x + j, d, PREFETCH_LEN,1); //load x
      }

      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      ISSUE_VINST(dotprod);
    }
    ISSUE_VINST(store_dp);
    for(int j=0; j<ny; j+=PREFETCH_LEN){

      sp_a_offset = spadRegion * REGION_SIZE;
      sp_ypart_offset = sp_a_offset + REGION_SIZE/2;

      for (int d = 0; d < dim; d++){
        VPREFETCH_L(sp_a_offset, a + _idx_(i+d,j,ny), d, PREFETCH_LEN ,1);
        VPREFETCH_L(sp_ypart_offset, _y_partial + ptid_group_sp[d]*ny +j, d, PREFETCH_LEN ,1); 
        //NOTE:in the worst case, there should be fence to stop prefetching before vector core writes to it, highly unlikely in this case
        // if (ptid==0) printf("prefetching for ptid %d, vtid %d via core %d\n",ptid_group_sp[d],d,ptid);
      }
      
      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      ISSUE_VINST(transpose_dp);
    }
    ISSUE_VINST(loop_end);
  }

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
  dotprod:
    asm("trillium glue_point dotprod");
  store_dp:
    asm("trillium glue_point store_dp");
  transpose_dp:
    asm("trillium glue_point transpose_dp");
  loop_end:
    asm("trillium glue_point loop_end");
  vector_stack:
    asm("trillium glue_point vector_stack");

  #elif defined VECTOR_CORE
  // init:;
  asm("trillium vissue_delim until_next init");
  volatile int bh1,bh2,bh3;
  
  int spadRegion =0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);

  int row_thread=nx_start+vtid;
  int col_thread=0;
  DTYPE temp;
  DTYPE* partialVec = _y_partial + ptid*ny;
  do {
    // hoist1:
    asm("trillium vissue_delim until_next hoist1");
    temp=0;
    do {

      // dotprod:
      asm("trillium vissue_delim until_next dotprod");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(4)
      for(int jj=0; jj<PREFETCH_LEN; jj++){
        DTYPE *a_on_sp = spAddr + spadRegion*REGION_SIZE + jj;
        DTYPE *x_on_sp = a_on_sp + REGION_SIZE/2;

        temp += (*a_on_sp) * (*x_on_sp);
      }
      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      REMEM(REGION_SIZE);
    } while(bh2);
    // store_dp:
    asm("trillium vissue_delim until_next store_dp");
    STORE_NOACK(temp, ax + row_thread, 0);
    col_thread=0;
    do {
      
      // transpose_dp:
      asm("trillium vissue_delim until_next transpose_dp");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(4)
      for(int jj=0; jj<PREFETCH_LEN; jj++){ 
        DTYPE *a_on_sp = spAddr + spadRegion*REGION_SIZE + jj;
        DTYPE *ypart_on_sp = a_on_sp + REGION_SIZE/2;
        DTYPE y_temp;

        y_temp = *ypart_on_sp + (*a_on_sp) * temp;
        STORE_NOACK(y_temp, partialVec + col_thread+jj, 0);
        // partialVec[col_thread+jj] = y_temp;
        // partialVec[col_thread+jj] += (*a_on_sp) * temp;
      }
      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      REMEM(REGION_SIZE);
      col_thread+=PREFETCH_LEN;
    } while(bh3);

    asm("trillium vissue_delim begin loop_end");
    row_thread+=dim;
    asm volatile("fence\n\t"); //since I have store noacks I don't move to next iter until all stores are done to partial vec
    asm("trillium vissue_delim end at_jump");
  } while (bh1);

  // mark vector stack cleanup assembly
  asm("trillium vissue_delim return vector_stack");
  return;
  #endif
  
  
}