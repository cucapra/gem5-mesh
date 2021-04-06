#include "atax_kernel.h"
// #define SCALAR_CORE
// #define VECTOR_CORE

#ifdef _VEC

inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

inline void prefetch_ax_frame(DTYPE *a, DTYPE *b, int i, int j, int n, int *spadRegion){

  int sp_a_offset = *spadRegion * REGION_SIZE;
  int sp_x_offset = sp_a_offset + REGION_SIZE/2;

  #ifdef LONGLINES
  VPREFETCH_LR(sp_a_offset, a + _idx_(i,j,n), 0, PREFETCH_LEN,TO_ALL_CORES); //load A, hopefully cache alligned so no vprefetch_R
  VPREFETCH_LR(sp_x_offset, b + j, 0, PREFETCH_LEN,TO_ALL_CORES); //load x
  #else     
  for (int d = 0; d < VECTOR_LEN; d++){
    VPREFETCH_L(sp_a_offset, a + _idx_(i+d,j,n), d, PREFETCH_LEN,TO_ONE_CORE); //load A, hopefully cache alligned so no vprefetch_R
    VPREFETCH_L(sp_x_offset, b + j, d, PREFETCH_LEN,TO_ONE_CORE); //load x
  }
  #endif
  *spadRegion = (*spadRegion + 1) % NUM_REGIONS;
}

inline void prefetch_ay_frame(DTYPE *a, DTYPE *b, int i, int j, int n, int *ptid_group_sp, int *spadRegion){

  int sp_a_offset = *spadRegion * REGION_SIZE;
  int sp_ypart_offset = sp_a_offset + REGION_SIZE/2;

  for (int d = 0; d < VECTOR_LEN; d++){
    VPREFETCH_L(sp_a_offset, a + _idx_(i+d,j,n), d, PREFETCH_LEN ,TO_ONE_CORE);
    VPREFETCH_L(sp_ypart_offset, b + ptid_group_sp[d]*n +j, d, PREFETCH_LEN ,TO_ONE_CORE); 
  }
  
  *spadRegion = (*spadRegion + 1) % NUM_REGIONS;
}


inline void prefetch_horizontal(DTYPE *a, DTYPE *ax, int i, int j, int n, int *spadRegion){

  int sp_a_offset = *spadRegion * REGION_SIZE;
  int sp_ax_offset = sp_a_offset + PREFETCH_LEN;
  
  for(int u=0; u<PREFETCH_LEN; u++){
    VPREFETCH_LR(sp_a_offset+u, a + _idx_(i+u,j,n), 0, 1 ,TO_ALL_CORES);
  }
  for (int d = 0; d < VECTOR_LEN; d++){
    VPREFETCH_LR(sp_ax_offset, ax +i, d, PREFETCH_LEN ,TO_ONE_CORE); 
  }
  
  *spadRegion = (*spadRegion + 1) % NUM_REGIONS;
}

#if defined(REDUCE_VERSION)

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

  int startOffset = INIT_FRAMES*PREFETCH_LEN;

  DTYPE temp;
  for (int i = nx_start; i < nx_end; i+=dim) {
    temp=0;
    ISSUE_VINST(hoist1);

    //init
    for (int j = 0; j < startOffset; j+=PREFETCH_LEN) prefetch_ax_frame(a,_x, i,j, ny, &spadRegion);

    //steady state
    for(int j=startOffset; j<ny; j+=PREFETCH_LEN){
      prefetch_ax_frame(a,_x, i,j, ny, &spadRegion);
      ISSUE_VINST(dotprod);
    }

    //final vissue
    for (int j = 0; j < startOffset; j+=PREFETCH_LEN) ISSUE_VINST(dotprod);

    ISSUE_VINST(store_dp);

    //init
    for(int j = 0; j < startOffset; j+=PREFETCH_LEN) prefetch_ay_frame(a, _y_partial, i, j, ny, ptid_group_sp, &spadRegion);

    //steady state
    for(int j=startOffset; j<ny; j+=PREFETCH_LEN){

      prefetch_ay_frame(a, _y_partial, i, j, ny, ptid_group_sp, &spadRegion);
      ISSUE_VINST(transpose_dp);
    }

    //final vissue
    for(int j = 0; j < startOffset; j+=PREFETCH_LEN) ISSUE_VINST(transpose_dp);

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

  #ifdef PER_CORE_SIMD
  vsetvl_e32m1(HARDWARE_VECTOR_LEN);
  #endif

  do {
    // hoist1:
    asm("trillium vissue_delim until_next hoist1");
    temp=0;
    do {
      
      #ifdef PER_CORE_SIMD
  vsetvl_e32m1(HARDWARE_VECTOR_LEN);
  #endif
      // dotprod:
      asm("trillium vissue_delim until_next dotprod");
      FRAME_START(REGION_SIZE);

      #pragma GCC unroll(16)
      for(int jj=0; jj<PREFETCH_LEN; jj+=PER_CORE_SIMD_LEN){
        #ifdef PER_CORE_SIMD

        vfloat32m1_t vx = vle32_v_f32m1(&spAddr[spadRegion*REGION_SIZE+REGION_SIZE/2+jj]);
        vfloat32m1_t va = vle32_v_f32m1(&spAddr[spadRegion*REGION_SIZE + jj]);

        // multiple together
        vfloat32m1_t vtemp = vfmul_vv_f32m1(va, vx);

        // sum
        vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
        vfloat32m1_t vs = vfredsum_vs_f32m1_f32m1(vtemp, vtemp, vzero);

        // update the accumulation
        float single_val = vfmv_f_s_f32m1_f32(vs);
        temp += single_val;
        #else
        DTYPE *a_on_sp = spAddr + spadRegion*REGION_SIZE + jj;
        DTYPE *x_on_sp = a_on_sp + REGION_SIZE/2;

        temp += (*a_on_sp) * (*x_on_sp);
        #endif
      }
      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      REMEM(REGION_SIZE);
    } while(bh2);
    // store_dp:
    asm("trillium vissue_delim until_next store_dp");
    FSTORE_NOACK(temp, ax + row_thread, 0);
    col_thread=0;
    do {
      
      #ifdef PER_CORE_SIMD
  vsetvl_e32m1(HARDWARE_VECTOR_LEN);
  #endif
      // transpose_dp:
      asm("trillium vissue_delim until_next transpose_dp");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(16)
      for(int jj=0; jj<PREFETCH_LEN; jj+=PER_CORE_SIMD_LEN){ 
        #ifdef PER_CORE_SIMD
        vfloat32m1_t va = vle32_v_f32m1(&spAddr[spadRegion*REGION_SIZE + jj]);
        vfloat32m1_t vpp = vle32_v_f32m1(&spAddr[spadRegion*REGION_SIZE + jj+ REGION_SIZE/2]);

        // multiple together
        vfloat32m1_t vp = vfmul_vf_f32m1(va, temp);
        vpp = vfadd_vv_f32m1(vpp, vp);
        vse32_v_f32m1(&partialVec[col_thread+jj], vpp);
        #else
        DTYPE *a_on_sp = spAddr + spadRegion*REGION_SIZE + jj;
        DTYPE *ypart_on_sp = a_on_sp + REGION_SIZE/2;
        DTYPE y_temp;

        y_temp = *ypart_on_sp + (*a_on_sp) * temp;
        FSTORE_NOACK(y_temp, partialVec + col_thread+jj, 0);
        // partialVec[col_thread+jj] = y_temp;
        // partialVec[col_thread+jj] += (*a_on_sp) * temp;
        #endif
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


#elif defined POLYBENCH_VERSION


void tril_atax1(int mask, DTYPE *a, DTYPE *_x, DTYPE *ax, int nx, int ny,
      int nx_start, int nx_end, int ptid, int vtid, int ptidMailer, int linkId)
{

  
  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);
  ISSUE_VINST(init); // issue vector block early so that vector cores don't stay idol
  
  //prefetch variables
  int spadRegion = 0;
  int sp_a_offset, sp_x_offset, sp_ypart_offset;

  int startOffset = min(INIT_FRAMES*J_STRIDE, ny);

  #ifdef LONGLINES
  int numCompleted = 0;
  volatile int *sp_ptr = (int*)getSpAddr(ptid, 0);
  #endif

  for (int i = nx_start; i < nx_end; i+=I_STRIDE) {
    ISSUE_VINST(hoist1);

    //init
    for (int j = 0; j < startOffset; j+=J_STRIDE) {
      prefetch_ax_frame(a,_x, i,j, ny, &spadRegion);
    }

    //steady state
    for(int j=startOffset; j<ny; j+=J_STRIDE){
      prefetch_ax_frame(a,_x, i,j, ny, &spadRegion);
      ISSUE_VINST(dotprod);
    }

    //final vissue
    for (int j = 0; j < startOffset; j+=J_STRIDE) {
      ISSUE_VINST(dotprod);
    }

    #ifdef LONGLINES
    SCALAR_SYNC_WITH_REDUCTION(sp_ptr, numCompleted);
    #endif

    ISSUE_VINST(store_dp);
  }

  ISSUE_VINST(vector_stack);
  // devec with unique tag
  DEVEC(devec_0);

  asm volatile("fence\n\t");

  asm("trillium vissue_delim return scalar_return"); 
  return;

  
  init:
    asm("trillium glue_point init");
    exit(1);
  hoist1:
    asm("trillium glue_point hoist1");
    exit(1);
  dotprod:
    asm("trillium glue_point dotprod");
    exit(1);
  store_dp:
    asm("trillium glue_point store_dp");
    exit(1);
  vector_stack:
    asm("trillium glue_point vector_stack");
    exit(1);

  #elif defined VECTOR_CORE
  // init:;
  asm("trillium vissue_delim until_next init");
  volatile int bh1,bh2;
  
  int spadRegion =0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);

  #ifdef LONGLINES
  // int row_thread=nx_start;
  // int col_thread = vtid*PREFETCH_LEN;
  int sp_origin = (linkId * PER_CORE_MAILER_FRAME) + vtid;
  DTYPE* sp_origin_ptr = (DTYPE*)getSpAddr(ptidMailer, 0);
  #else
  int row_thread=nx_start+vtid;
  int col_thread=0;
  #endif
  do {
    // hoist1:
    asm("trillium vissue_delim until_next hoist1");
    DTYPE temp=0;
    do {

      // dotprod:
      asm("trillium vissue_delim until_next dotprod");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(16)
      for(int jj=0; jj<PREFETCH_LEN; jj+=PER_CORE_SIMD_LEN){
        DTYPE *a_on_sp = spAddr + spadRegion*REGION_SIZE + jj;
        DTYPE *x_on_sp = a_on_sp + REGION_SIZE/2;

        temp += (*a_on_sp) * (*x_on_sp);
      }
      spadRegion = (spadRegion + 1) % NUM_REGIONS;
      REMEM(REGION_SIZE);
    } while(bh2);
    // store_dp:
    asm("trillium vissue_delim until_next store_dp");

    #ifdef LONGLINES
    FSTORE_NOACK(temp, &sp_origin_ptr[sp_origin], 0);
    sp_origin+=SUB_FRAME_SIZE;
    sp_origin = sp_origin % MAILER_POST_FRAME_WORD;
    #else
    FSTORE_NOACK(temp, ax + row_thread, 0);
    row_thread+=I_STRIDE;
    #endif
  } while (bh1);

  // mark vector stack cleanup assembly
  asm("trillium vissue_delim return vector_stack");
  return;
  #endif
  
  
}

void tril_atax2(int mask, DTYPE *a, DTYPE *ax, DTYPE *_y, int nx, int ny,
      int ny_start, int ny_end, int ptid, int vtid, int dim)
{

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);
  ISSUE_VINST(init); // issue vector block early so that vector cores don't stay idol
  
  //prefetch variables
  int spadRegion = 0;
  int sp_a_offset, sp_ax_offset;

  int startOffset = INIT_FRAMES*PREFETCH_LEN;

  DTYPE temp;
  for (int j = ny_start; j < ny_end; j+=dim) {
    temp=0;
    ISSUE_VINST(hoist1);

    //init
    for (int i = 0; i < startOffset; i+=PREFETCH_LEN) prefetch_horizontal(a,ax, i,j, ny, &spadRegion);

    //steady state
    for(int i=startOffset; i<nx; i+=PREFETCH_LEN){
      prefetch_horizontal(a,ax, i,j, ny, &spadRegion);
      ISSUE_VINST(dotprod);
    }

    //final vissue
    for (int i = 0; i < startOffset; i+=PREFETCH_LEN) ISSUE_VINST(dotprod);

    ISSUE_VINST(store_dp);

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
  vector_stack:
    asm("trillium glue_point vector_stack");

  #elif defined VECTOR_CORE
  // init:;
  asm("trillium vissue_delim until_next init");
  volatile int bh1,bh2,bh3;
  
  int spadRegion =0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);

  int col_thread=ny_start+vtid;
  int row_thread=0;
  DTYPE temp;
  do {
    // hoist1:
    asm("trillium vissue_delim until_next hoist1");
    temp=0;
    do {

      // dotprod:
      asm("trillium vissue_delim until_next dotprod");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(16)
      for(int ii=0; ii<PREFETCH_LEN; ii++){
        DTYPE *a_on_sp = spAddr + spadRegion*REGION_SIZE + ii;
        DTYPE *ax_on_sp = a_on_sp + REGION_SIZE/2;

        temp += (*a_on_sp) * (*ax_on_sp);
      }
      REMEM(REGION_SIZE);
      spadRegion = (spadRegion + 1) % NUM_REGIONS;
    } while(bh2);
    // store_dp:
    asm("trillium vissue_delim until_next store_dp");
    FSTORE_NOACK(temp, _y + col_thread, 0);
    col_thread+=dim;
    asm volatile("fence\n\t"); //since I have store noacks I don't move to next iter until all stores are done to partial vec
  } while (bh1);

  // mark vector stack cleanup assembly
  asm("trillium vissue_delim return vector_stack");
  return;
  #endif

}

#endif

#endif