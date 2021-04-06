#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "atax.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "util.h"

#include "atax_kernel.h"

#ifdef PER_CORE_SIMD
#include <riscv_vector.h>
#endif

static inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

#ifdef LONGLINES
// partial sum reduction offload
void atax1_reduction(DTYPE *out, int baseGroupId, int numGroups, int N, 
    int ptid, int *fwders) {
  // chunk over vector groups. note all might not do the same amount of work
  int max_chunk_size = ceilToInt((float)N / (float)numGroups);

  // cache sp ptrs to avoid global load
  SETUP_REDUCTION_CORE(fwders, ptid, N, baseGroupId, numGroups);

  for (int cnt = 0; cnt < max_chunk_size; cnt+=ACCUM_GRANULARITY) {

    SETUP_GROUP_ITERATION_CHUNKED_1NEST(baseGroupId, numGroups, cnt, max_chunk_size);

    REDUCE_SYNC_WITH_SCALAR(group_start, spPtrs, flat_iter);

    // wait for frame and then do sum
    FRAME_START(expected_elements);

    for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {

      #pragma GCC unroll(4)
      for (int a = 0; a < ACCUM_GRANULARITY; a++) {
        DTYPE sum = 0.0f;

        int sum_offset = g * PER_CORE_MAILER_FRAME + a * SUB_FRAME_SIZE;

        #pragma GCC unroll(16)
        for (int k = 0; k < PER_CORE_MAILER_FRAME; k++) {
          sum += sp_ptr[sum_offset + sp_self + k];
        }

        int i = group_start[g];

        if (i < 0 || i + a >= group_end[g]) continue;

        FSTORE_NOACK(sum, &out[i + a], 0);
      }

    }
    sp_self += MAILER_FRAME_SIZE;
    sp_self = sp_self % MAILER_POST_FRAME_WORD; // TOOD branch better??
    REMEM(expected_elements);
  }
}
#endif

#ifdef REDUCE_VERSION
void __attribute__((optimize("-fno-inline")))
atax_manycore(DTYPE *a, DTYPE *_x, DTYPE *_y_partial, DTYPE *ax, int nx, int ny,
      int nx_start, int nx_end, int ptid)
{
    DTYPE temp;
    DTYPE *partial_prod = _y_partial + ptid*ny;

    #ifdef MANYCORE_PREFETCH
    int spadRegion = 0;
    DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);
    int sp_a_offset,sp_x_offset,sp_partial_offset;
    #endif

    for (int i = nx_start; i < nx_end; i++) {
      temp=0;
      #ifdef MANYCORE_PREFETCH
      for(int j=0; j<ny; j+=PREFETCH_LEN){
        sp_a_offset = spadRegion * REGION_SIZE;
        sp_x_offset = sp_a_offset + PREFETCH_LEN;

        VPREFETCH_L(sp_a_offset, a + _idx_(i, j, ny), 0, PREFETCH_LEN,TO_SELF);
        VPREFETCH_L(sp_x_offset, _x + j, 0, PREFETCH_LEN,TO_SELF);
        FRAME_START(REGION_SIZE);
        #pragma GCC unroll(16)
        for(int jj=0; jj<PREFETCH_LEN; jj++){
          temp+=spAddr[sp_a_offset+jj]*spAddr[sp_x_offset+jj];
        }
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
        REMEM(REGION_SIZE);
      }
      #elif defined(PER_CORE_SIMD)
      int chunk = ny;
      for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
        l = vsetvl_e32m1(chunk);
        int j = ny - chunk;

        vfloat32m1_t vx = vle32_v_f32m1(&_x[j]);
        vfloat32m1_t va = vle32_v_f32m1(&a[i*ny+j]);

        // multiple together
        vfloat32m1_t vtemp = vfmul_vv_f32m1(va, vx);

        // sum
        vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
        vfloat32m1_t vs = vfredsum_vs_f32m1_f32m1(vtemp, vtemp, vzero);

        // update the accumulation
        float single_val = vfmv_f_s_f32m1_f32(vs);
        temp += single_val;
      }
      #else
      #pragma GCC unroll(16)
      for(int j=0; j<ny; j++){
        temp += a[i*ny+j] * _x[j];
      }
      #endif
      STORE_NOACK(temp, ax + i, 0);

      #ifdef MANYCORE_PREFETCH
      for(int j=0; j<ny; j+=PREFETCH_LEN){
        sp_a_offset = spadRegion * REGION_SIZE;
        sp_partial_offset = sp_a_offset + PREFETCH_LEN;

        VPREFETCH_L(sp_a_offset, a + _idx_(i, j, ny), 0, PREFETCH_LEN,TO_SELF);
        VPREFETCH_L(sp_partial_offset, partial_prod + j, 0, PREFETCH_LEN,TO_SELF);
        FRAME_START(REGION_SIZE);
        #pragma GCC unroll(16)
        for(int jj=0; jj<PREFETCH_LEN; jj++){
          DTYPE partial_temp= spAddr[sp_partial_offset+jj] + spAddr[sp_a_offset+jj]*temp;
          STORE_NOACK(partial_temp, partial_prod + j+jj, 0);
        }
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
        REMEM(REGION_SIZE);
      }
      #elif defined(PER_CORE_SIMD)
      chunk = ny;
      for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
        l = vsetvl_e32m1(chunk);
        int j = ny - chunk;

        vfloat32m1_t va = vle32_v_f32m1(&a[i*ny+j]);
        vfloat32m1_t vpp = vle32_v_f32m1(&partial_prod[j]);

        // multiple together
        vfloat32m1_t vp = vfmul_vf_f32m1(va, temp);
        vpp = vfadd_vv_f32m1(vpp, vp);


        vse32_v_f32m1(&partial_prod[j], vpp);
      }
      #else
      #pragma GCC unroll(16)
      for(int j=0; j<ny; j++){
        partial_prod[j] += a[i*ny+j] * temp;
      }
      #endif
    }
}

void reduce_parallel(DTYPE* partial, DTYPE *out, int n, int ptid, int pdim, int unique_id, int total_groups, int vtid,
                      int vdim_x, int vdim_y, int phys_dim_x){

  #ifndef _VEC
  //cores are used
  int start = (ptid + 0) * n / pdim; 
  int end = (ptid + 1) * n / pdim;

  DTYPE temp;
  for(int i=start; i<end; i++){
    temp=0;
    for(int j=0; j<pdim; j++){
      temp+=partial[j*n+i];
    }
    out[i]+=temp;
  }

  #else
  int start = (unique_id + 0) * n / total_groups;
  int end = (unique_id + 1) * n / total_groups;
  start+=vtid;

  DTYPE temp;
  for(int i=start; i<end; i+=VECTOR_LEN){
    temp=0;
    for(int j=0; j<pdim; j++){
      temp+=partial[j*n+i];
    }
    // for(int j=0; j<total_groups; j++){
    //   for(int x=0; x<vdim_x; x++){
    //     for(int y=0; y<vdim_y; y++){
    //       int p = get_ptid_from_group(tinfo, j, x, y, phys_dim_x);
    //       temp+=partial[p*n+i];
    //     }
    //   }
    // }
    out[i]+=temp;
  }
  #endif
  return;

}

#elif defined POLYBENCH_VERSION

void __attribute__((optimize("-fno-inline")))
atax_manycore1(DTYPE *a, DTYPE *_x, DTYPE *ax, int nx, int ny,
      int nx_start, int nx_end, int ptid)
{
    DTYPE temp;

    #ifdef MANYCORE_PREFETCH
    int spadRegion = 0;
    DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);
    int sp_a_offset,sp_x_offset,sp_partial_offset;
    #endif

    for (int i = nx_start; i < nx_end; i++) {
      temp=0;
      #ifdef MANYCORE_PREFETCH
      for(int j=0; j<ny; j+=PREFETCH_LEN){
        sp_a_offset = spadRegion * REGION_SIZE;
        sp_x_offset = sp_a_offset + PREFETCH_LEN;

        VPREFETCH_L(sp_a_offset, a + _idx_(i, j, ny), 0, PREFETCH_LEN,TO_SELF);
        VPREFETCH_L(sp_x_offset, _x + j, 0, PREFETCH_LEN,TO_SELF);
        FRAME_START(REGION_SIZE);
        for(int jj=0; jj<PREFETCH_LEN; jj++){
          temp+=spAddr[sp_a_offset+jj]*spAddr[sp_x_offset+jj];
        }
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
        REMEM(REGION_SIZE);
      }
      #else
      for(int j=0; j<ny; j++){
        temp += a[i*ny+j] * _x[j];
      }
      #endif
      FSTORE_NOACK(temp, ax + i, 0);
    }
}

void __attribute__((optimize("-fno-inline")))
atax_manycore2(DTYPE *a, DTYPE *ax, DTYPE *_y, int nx, int ny,
      int ny_start, int ny_end, int ptid)
{
    DTYPE temp;

    for (int j = ny_start; j < ny_end; j++) {
      temp=0;
      for(int i=0; i<nx; i++){
        temp += a[i*ny+j]*ax[i];
      }
      FSTORE_NOACK(temp, _y + j, 0);
    }
    
}



#endif

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *_x, DTYPE *_y, DTYPE *ax, DTYPE *_y_partial, int nx, int ny,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y)
{

  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0)
  {
    stats_on();
  }

  int start = 0;
  int end = 0;
  

  #ifdef _VEC
  #if VECTOR_LEN==4
  SET_USEFUL_VARIABLES_V4(ptid_x, ptid_y, pdim_x, pdim_y);
  #elif VECTOR_LEN==16
  SET_USEFUL_VARIABLES_V16(ptid_x, ptid_y, pdim_x, pdim_y);
  #endif

  int* ptid_group = getSpAddr(ptid,10);

  if(used){
    int alignment = VECTOR_LEN;
    start = roundUp((unique_id + 0) * nx / total_groups, alignment); 
    end = roundUp((unique_id + 1) * nx / total_groups, alignment); 

    if(cinfo.is_scalar==1){
      // printf("I'm a DA core:%d\n",ptid);
      for(int i=0; i<vdim_y;i++){
        for(int j=0; j<vdim_x; j++){
          ptid_group[i*vdim_x+j] = get_ptid_from_group(&tinfo, unique_id,j,i,pdim_x);
          // if (ptid==0) printf("Ptid: %d\n", ptid_group[i*vdim_x+j]);
        }
      }
      // sprintf("group id: %d, DA core:%d, Origin: %d\n", unique_id, ptid, ptid_group[0]);
    }
  }

  #else
  SET_USEFUL_VARIABLES_MANYCORE(ptid_x, ptid_y, pdim_x, pdim_y);
  
  start  = ( ( ptid + 0 ) * nx ) / pdim;
  end    = ( ( ptid + 1 ) * nx ) / pdim;
  #endif

  #ifdef LONGLINES
  SETUP_REDUCE_CONFIG();
  #else
  SETUP_REDUCE_CONFIG_NULL();
  #endif

  // need to set vlen here so doesn't cause squash in vector core on change in value
  #ifdef PER_CORE_SIMD
  vsetvl_e32m1(PER_CORE_SIMD_LEN);
  #endif

#ifdef NESTED_SIMD
  vsetvl_e32m1(HARDWARE_VECTOR_LEN);
  #endif
// do after tokens to avoid stalling due to region not ready
// region based mask for scratchpad
#ifdef _VEC
  if (isMailer) {
    SET_PREFETCH_MASK(MAILER_NUM_FRAMES, MAILER_FRAME_SIZE, &start_barrier);
  }
  else {
    SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
  }
#elif defined MANYCORE_PREFETCH
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
#endif

  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later

  unsigned long long *spTop = getSpTop(ptid);
  // // // guess the remaining of the part of the frame (n) that might be needed?? here n = 30
  spTop -= 60;

  unsigned long long stackLoc;
  unsigned long long temp;
  #pragma GCC unroll(60)
  for(int i=0;i<60;i++){
    asm volatile("ld t0, %[id](sp)\n\t"
                "sd t0, %[id](%[spad])\n\t"
                : "=r"(temp)
                : [id] "i"(i*8), [spad] "r"(spTop));
  }
  asm volatile (// save the stack ptr
      "addi %[dest], sp, 0\n\t"
      // overwrite stack ptr
      "addi sp, %[spad], 0\n\t"
      : [ dest ] "=r"(stackLoc)
      : [ spad ] "r"(spTop));

  // move stack onto scratchpad for faster local access than default on DRAM
  // TODO needs 60, but default is 30
  // MOVE_STACK_ONTO_SCRATCHPAD();

  #ifdef REDUCE_VERSION
  if(used!=0){
    #if defined _VEC
      tril_atax(mask,a,_x,_y_partial,ax,nx,ny,start,end,ptid,vtid,vdim,ptid_group);
    #else
      atax_manycore(a,_x,_y_partial,ax,nx,ny,start,end,ptid);
    #endif

  }

  //requires barrier since each core needs values from all other cores
  pthread_barrier_wait(&start_barrier);

  if (used == 0) goto stack_end; //return;
  #ifdef _VEC
  if (cinfo.is_scalar) goto stack_end; //return; // scalar cores don't have data to accumulate so should not partcipate
  #endif
  
  
  
  reduce_parallel(_y_partial, _y, ny, ptid, pdim, unique_id, total_groups, vtid,
                      vdim_x, vdim_y, pdim_x);

  #elif defined POLYBENCH_VERSION

  #if defined _VEC
  if (used)
    tril_atax1(mask,a,_x,ax,nx,ny,start,end,ptid,vtid,ptidMailer,linkId);
  #ifdef LONGLINES
  else if (isMailer)
    atax1_reduction(ax, unique_id, total_groups, nx, ptid, ptidFwders);
  #endif
  #else
  atax_manycore1(a,_x,ax,nx,ny,start,end,ptid);
  #endif

  #ifdef _VEC
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  #elif defined MANYCORE_PREFETCH
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
  #else
  pthread_barrier_wait(&start_barrier);
  #endif

  if(used!=0){
    #if defined _VEC
      tril_atax2(mask,a,ax,_y,nx,ny,start,end,ptid,vtid,vdim);
    #else
      atax_manycore2(a,ax,_y,nx,ny,start,end,ptid);
    #endif

  }

  #endif

  stack_end:
  // restore stack pointer to DRAM
  RECOVER_DRAM_STACK();
  return;

  
  
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *_x, DTYPE *_y, DTYPE *ax, DTYPE *_y_partial, 
                          int nx, int ny, int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->_x = _x;
  args->_y = _y;
  args->ax = ax;
  args->_y_partial = _y_partial;
  args->nx = nx;
  args->ny = ny;
  args->tid_x = tid_x;
  args->tid_y = tid_y;
  args->dim_x = dim_x;
  args->dim_y = dim_y;

  return args;
}

void *pthread_kernel(void *args)
{
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early

  pthread_barrier_wait(&start_barrier);

  // call the spmd kernel
  Kern_Args *a = (Kern_Args *)args;

  kernel(a->a, a->_x, a->_y, a->ax, a->_y_partial, a->nx, a->ny,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 1 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
