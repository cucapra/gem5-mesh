#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "mvt.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "util.h"

#include "mvt_kernel.h"

#ifdef PER_CORE_SIMD
#include <riscv_vector.h>
#endif

#define STACK_COPY


void __attribute__((optimize("-fno-inline")))
mvt_manycore(DTYPE *a, DTYPE *y1, DTYPE *y2, DTYPE *x1, DTYPE *x2, int n, int start, int end, int ptid)
{
  DTYPE temp;

  #ifdef MANYCORE_PREFETCH
  int spadRegion = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);
  int sp_a_offset, sp_y_offset;
  #endif

  #ifdef PER_CORE_SIMD
  vsetvl_e32m1(PER_CORE_SIMD_LEN);
  vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f);
  #endif

  for (int i = start; i < end; i++) {
      temp=0;
      #ifdef MANYCORE_PREFETCH
      #ifdef PER_CORE_SIMD
      vsetvl_e32m1(PER_CORE_SIMD_LEN);
      vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f);
      vfloat32m1_t accum = vzero;
      #endif

      PF_BEGIN(REGION_SIZE/2)
      PF2(sp_a_offset,sp_y_offset,a,y1,i*n+j,j)
      {
        #ifdef PER_CORE_SIMD
        vfloat32m1_t va = vle32_v_f32m1(&spAddr[sp_a_offset+jj]);
        vfloat32m1_t vy1 = vle32_v_f32m1(&spAddr[sp_y_offset+jj]);
        vfloat32m1_t vay1 = vfmul_vv_f32m1(va, vy1);
        accum = vfadd_vv_f32m1(accum, vay1);
        #else
        temp+= spAddr[sp_a_offset+jj]*spAddr[sp_y_offset+jj];
        #endif
      }
      PF_END(NUM_REGIONS)

      #ifdef PER_CORE_SIMD
      vsetvl_e32m1(PER_CORE_SIMD_LEN);
      vfloat32m1_t vtempred = vfredsum_vs_f32m1_f32m1(accum, accum, vzero);
      temp += vfmv_f_s_f32m1_f32(vtempred);
      #endif

      #elif defined(PER_CORE_SIMD)
      int chunk = n;
      for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
        l = vsetvl_e32m1(chunk);
        int j = n - chunk;

        vfloat32m1_t va = vle32_v_f32m1(&a[i*n+j]);
        vfloat32m1_t vy1 = vle32_v_f32m1(&y1[j]);

        vfloat32m1_t vay1 = vfmul_vv_f32m1(va, vy1);

        // sum
        vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
        vfloat32m1_t vs = vfredsum_vs_f32m1_f32m1(vay1, vay1, vzero);

        // update the accumulation
        float single_val = vfmv_f_s_f32m1_f32(vs);
        temp += single_val;
      }
      #else
      #pragma GCC unroll(16)
      for(int j=0; j<n; j++){
        temp += a[i*n+j] * y1[j];
      }
      #endif
      x1[i]+=temp;
      temp=0;
      #ifdef MANYCORE_PREFETCH

      #ifdef PER_CORE_SIMD
      vsetvl_e32m1(PER_CORE_SIMD_LEN);
      accum = vzero;
      #endif

      PF_BEGIN(REGION_SIZE/2)
      PF1(sp_y_offset,sp_a_offset,y2,a,j,i,n)
      {
        #ifdef PER_CORE_SIMD
        vfloat32m1_t va = vle32_v_f32m1(&spAddr[sp_a_offset + jj]);
        vfloat32m1_t vy2 = vle32_v_f32m1(&spAddr[sp_y_offset+jj]);
        vfloat32m1_t vay2 = vfmul_vv_f32m1(va, vy2);
        accum = vfadd_vv_f32m1(accum, vay2);
        #else
        // temp+=a[(j+jj)*n+i] * spAddr[sp_y_offset+jj];
        temp+=spAddr[sp_a_offset+jj] * spAddr[sp_y_offset+jj];
        
        #endif
      }
      PF_END(NUM_REGIONS)

      #ifdef PER_CORE_SIMD
      vsetvl_e32m1(PER_CORE_SIMD_LEN);
      vtempred = vfredsum_vs_f32m1_f32m1(accum, accum, vzero);
      temp += vfmv_f_s_f32m1_f32(vtempred);
      #endif

      #elif defined(PER_CORE_SIMD)
      chunk = n;
      for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
        l = vsetvl_e32m1(chunk);
        int j = n - chunk;

        vfloat32m1_t va = vlse32_v_f32m1(&a[j*n+i], n * sizeof(float));
        vfloat32m1_t vy2 = vle32_v_f32m1(&y2[j]);

        vfloat32m1_t vay2 = vfmul_vv_f32m1(va, vy2);

        // sum
        vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
        vfloat32m1_t vs = vfredsum_vs_f32m1_f32m1(vay2, vay2, vzero);

        // update the accumulation
        float single_val = vfmv_f_s_f32m1_f32(vs);
        temp += single_val;
      }
      #else
      #pragma GCC unroll(16)
      for(int j=0; j<n; j++){
        temp+= a[j*n+i] * y2[j];
      }
      #endif
      // x2[i]+=temp;
      FSTORE_NOACK(x2[i] + temp, &x2[i], 0);
  }
}

void __attribute__((optimize("-fno-inline")))
mvt_main(int mask, DTYPE *a, DTYPE *y1, DTYPE *y2, DTYPE *x1, DTYPE *x2, int n, 
                  int start, int end, int ptid, int pdim, int pdim_x, int used, int vtid){

  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later
  
  #ifdef STACK_COPY
  MOVE_STACK_ONTO_SCRATCHPAD();
  #endif

  if(used!=0){
    // if(ptid==0)printf("2. ptid pointer %x\n",ptid_group);
    #if defined _VEC
      tril_mvt_vec(mask,a,y1,y2,x1,x2,n,start,end,ptid, vtid);

    #else
      VECTOR_EPOCH(mask);
      mvt_manycore(a,y1,y2,x1,x2,n,start,end,ptid);
    #endif
  }

  #ifdef STACK_COPY
  RECOVER_DRAM_STACK();
  #endif
}

void kernel(DTYPE *a, DTYPE *y1, DTYPE *y2, DTYPE *x1, DTYPE *x2, int n,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y)
{

  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0)
  {
    stats_on();
  }
  int start = 0;
  int end = 0;
  // int* ptid_group = getSpAddr(ptid,REGION_SIZE*NUM_REGIONS);

  #ifdef _VEC
  #if VECTOR_LEN==4
  SET_USEFUL_VARIABLES_V4(ptid_x, ptid_y, pdim_x, pdim_y);
  #elif VECTOR_LEN==16
  SET_USEFUL_VARIABLES_V16(ptid_x, ptid_y, pdim_x, pdim_y);
  #endif

  // if(ptid==0) printf("Total groups %d\n",cinfo.total_groups);
  if(used){
    //do work division here
    int alignment = VECTOR_LEN; //each group should have elements of multiple of this number
    start = roundUp((unique_id + 0) * n / total_groups, alignment); 
    end = roundUp((unique_id + 1) * n / total_groups, alignment); 

    // if(cinfo.is_scalar==1) printf("ptid:%d, start=%d and end=%d\n",ptid,start,end);
  }

  #else
  SET_USEFUL_VARIABLES_MANYCORE(ptid_x, ptid_y, pdim_x, pdim_y);
  
  //do work division here
  start  = ( ( ptid + 0 ) * n ) / pdim;
  end    = ( ( ptid + 1 ) * n ) / pdim;
  #endif

#ifdef PER_CORE_SIMD
  vsetvl_e32m1(HARDWARE_VECTOR_LEN);
  #endif
  // region based mask for scratchpad
#ifdef _VEC
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
#elif defined MANYCORE_PREFETCH
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
#endif

// only let certain tids continue
  // if (used == 0) return;

  mvt_main(mask,a,y1,y2,x1,x2,n,start,end,ptid, pdim, pdim_x,used,vtid);
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *y1, DTYPE *y2, DTYPE *x1, DTYPE *x2, int n,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->y1 = y1;
  args->y2 = y2;
  args->x1 = x1;
  args->x2 = x2;
  args->n = n;
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

  kernel(a->a, a->y1, a->y2, a->x1, a->x2, a->n,
          a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  // reset scratchpad config
  SET_PREFETCH_MASK(0, 0, &start_barrier);
  
  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
