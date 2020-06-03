#include "atax_kernel.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

static inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

void atax_vec(int mask, DTYPE *a, DTYPE *_x, DTYPE *_y_partial, DTYPE *ax, int nx, int ny,
      int nx_start, int nx_end, int ptid, int vtid, int dim)
{

  VECTOR_EPOCH(mask);

  #ifdef SCALAR_CORE
  ISSUE_VINST(init); // issue vector block early so that vector cores don't stay idol
  
  //prefetch variables
  int spadRegion = 0;
  int sp_a_offset, sp_x_offset;

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
    for(int j=0; j<ny; j+=REGION_SIZE){

      sp_a_offset = spadRegion * REGION_SIZE;
      for (int d = 0; d < dim; d++) VPREFETCH_L(sp_a_offset, a + _idx_(i+d,j,ny), d, REGION_SIZE ,1);
      
      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      ISSUE_VINST(transpose_dp);
    }
    ISSUE_VINST(loop_end);
  }

  ISSUE_VINST(stack_end);
  // devec with unique tag
  DEVEC(devec_0);


  #elif defined VECTOR_CORE
  init:;
  volatile int bh1,bh2,bh3;
  
  int spadRegion =0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);

  int row_thread=nx_start+vtid;
  int col_thread=0;
  DTYPE temp;
  DTYPE* partialVec = _y_partial + ptid*ny;
  while(bh1){
    hoist1:
    temp=0;
    while(bh2){

      dotprod:
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(4)
      for(int jj=0; jj<PREFETCH_LEN; jj++){
        DTYPE *a_on_sp = spAddr + spadRegion*REGION_SIZE + jj;
        DTYPE *x_on_sp = a_on_sp + REGION_SIZE/2;

        temp += (*a_on_sp) * (*x_on_sp);
      }
      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      REMEM(REGION_SIZE);
    }
    store_dp:
    STORE_NOACK(temp, ax + row_thread, 0);
    col_thread=0;
    while(bh3){
      
      transpose_dp:
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(8)
      for(int jj=0; jj<REGION_SIZE; jj++){
        DTYPE *a_on_sp = spAddr + spadRegion*REGION_SIZE + jj;

        partialVec[col_thread+jj] += (*a_on_sp) * temp;
      }
      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      REMEM(REGION_SIZE);
      col_thread+=REGION_SIZE;
    }

    row_thread+=dim;
  }
  #endif

  asm volatile("fence\n\t");

  return;

  #ifdef SCALAR_CORE
  init:
    asm("nop");
  hoist1:
    asm("nop");
  dotprod:
    asm("nop");
  store_dp:
    asm("nop");
  transpose_dp:
    asm("nop");
  loop_end:
    asm("nop");
  stack_end:
    asm("nop");

  return;
  #endif
}