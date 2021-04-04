#include <stdio.h>

#include "pthread_launch.h"
#include "gesummv.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "util.h"

#include "gesummv_kernel.h"

#if defined(PACKED_SIMD) || defined(NESTED_SIMD) 
#include <riscv_vector.h>
#endif

void gesummv_manycore(DTYPE *a, DTYPE *b, DTYPE *x, DTYPE *tmp, DTYPE *y, int n, int start, int end, int ptid)
{
  
  DTYPE temp1, temp2;

  #ifdef MANYCORE_PREFETCH
  int spadRegion = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);
  int sp_a_offset, sp_b_offset, sp_x_offset;
  #endif

  for (int i = start; i < end; i++) {
      
      // TODO make sure these loads are negligable (all versions do fyi)
      temp1=tmp[i];
      temp2=y[i];
      
      #ifdef MANYCORE_PREFETCH
      for (int j = 0; j < n; j+=REGION_SIZE/3){
        sp_a_offset = spadRegion * REGION_SIZE;
        VPREFETCH_L(sp_a_offset, a + i*n+j, 0, REGION_SIZE/3,TO_SELF);

        sp_b_offset = sp_a_offset + (REGION_SIZE/3);
        VPREFETCH_L(sp_b_offset, b + i*n+j, 0, REGION_SIZE/3,TO_SELF);

        sp_x_offset = sp_b_offset + (REGION_SIZE/3);        
        VPREFETCH_L(sp_x_offset, x+j, 0, REGION_SIZE/3,TO_SELF);

        FRAME_START(REGION_SIZE);
        #pragma GCC unroll(8)
        for(int jj=0; jj<REGION_SIZE/3; jj++){
          temp1 += spAddr[sp_a_offset+jj]*spAddr[sp_x_offset+jj];
          temp2 += spAddr[sp_b_offset+jj]*spAddr[sp_x_offset+jj];
        }
        REMEM();
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
      }
      #elif defined(PACKED_SIMD)
      int chunk = n;
      for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
        l = vsetvl_e32m1(chunk);
        int j = n - chunk;

        vfloat32m1_t va = vle32_v_f32m1(&a[i*n+j]);
        vfloat32m1_t vb = vle32_v_f32m1(&b[i*n+j]);
        vfloat32m1_t vx = vle32_v_f32m1(&x[j]);

        va = vfmul_vv_f32m1(va, vx);
        vb = vfmul_vv_f32m1(vb, vx);

        // sum
        vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
        vfloat32m1_t vsa = vfredsum_vs_f32m1_f32m1(va, va, vzero);
        vfloat32m1_t vsb = vfredsum_vs_f32m1_f32m1(vb, vb, vzero);

        // update the accumulation
        float temp1_val = vfmv_f_s_f32m1_f32(vsa);
        temp1 += temp1_val;
        float temp2_val = vfmv_f_s_f32m1_f32(vsb);
        temp2 += temp2_val;
      }
      #else
      #pragma GCC unroll(8)
      for(int j=0; j<n; j++){
        temp1 += a[i*n+j] * x[j];
        temp2 += b[i*n+j] * x[j];
      }
      #endif
      FSTORE_NOACK(temp1, &tmp[i], 0);
      DTYPE y_i = ALPHA*temp1 + BETA*temp2;
      FSTORE_NOACK(y_i, &y[i], 0);
  }
  FENCE();
}

void __attribute__((optimize("-fno-inline"))) gesummv(
    DTYPE *a, DTYPE *b, DTYPE *tmp, DTYPE *x, DTYPE *y, int n,
    int mask, int start, int end, int ptid, int vtid
  ) {

  #if defined _VEC
    tril_gesummv_vec(mask,a,b,x,tmp,y,n,start,end,ptid,vtid);
  #else
    gesummv_manycore(a,b,x,tmp,y,n,start,end,ptid);
  #endif

}



void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(DTYPE *a, DTYPE *b, DTYPE *x, DTYPE *tmp, DTYPE *y, int n,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y)
{

  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0) {
    stats_on();
  }

  int start = 0;
  int end   = 0;

  #ifdef _VEC
  #if VEC_LEN==4
  SET_USEFUL_VARIABLES_V4(ptid_x, ptid_y, pdim_x, pdim_y);
  #elif VEC_LEN==16
  SET_USEFUL_VARIABLES_V16(ptid_x, ptid_y, pdim_x, pdim_y);
  #endif

  if(used) {
    //do work division here
    int alignment = VEC_LEN; //each group should have elements of multiple of this number
    start = roundUp((unique_id + 0) * n / total_groups, alignment); 
    end = roundUp((unique_id + 1) * n / total_groups, alignment); 
  }

  #else
  SET_USEFUL_VARIABLES_MANYCORE(ptid_x, ptid_y, pdim_x, pdim_y);

  //do work division here
  start  = ( ( unique_id + 0 ) * n ) / total_groups;
  end    = ( ( unique_id + 1 ) * n ) / total_groups;
  #endif

  // region based mask for scratchpad
#ifdef _VEC
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
#elif defined MANYCORE_PREFETCH
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
#endif

  // only let certain tids continue
  if (used == 0) return;

  // move stack onto scratchpad for faster local access than default on DRAM
  MOVE_STACK_ONTO_SCRATCHPAD();

  gesummv(a, b, tmp, x, y, n, mask, start, end, ptid, vtid);

  // restore stack pointer to DRAM
  RECOVER_DRAM_STACK();
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, DTYPE *x, DTYPE *tmp, DTYPE *y, int n,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->b = b;
  args->x = x;
  args->tmp = tmp;
  args->y = y;
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

  kernel(a->a, a->b, a->x, a->tmp, a->y, a->n,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  // reset scratchpad config
  SET_PREFETCH_MASK(0, 0, &start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
