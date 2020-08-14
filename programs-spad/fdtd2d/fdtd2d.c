#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "fdtd2d.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "fdtd2d_kernel.h"

/*
  FDTD-2D
*/

/*-----------------------------------------------------------------------------------
 * Manycore. Using PolyBench GPU parallelization strategy. No scratchpad use
 *---------------------------------------------------------------------------------*/

void fdtd_step1_manycore(DTYPE *fict, DTYPE *ex, DTYPE *ey, DTYPE *hz, int t, int NX, int NY, int tid, int dim) {
  int start = ((tid + 0) * NX) / dim;
  int end   = ((tid + 1) * NX) / dim;

  #ifdef MANYCORE_PREFETCH
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);
  for (int i = start; i < end; i++) {
    if (i == 0) {
      for (int j = 0; j < NY; j+=STEP1_UNROLL_LEN) {     
        prefetch_step1_frame_i0(fict, t, &sp);
        START_FRAME();
        #pragma GCC unroll(4)
        for (int u = 0; u < STEP1_UNROLL_LEN; u++) {
          DTYPE out = sp_ptr[sp + 0];
          STORE_NOACK(out, &ey[i * NY + j + u], 0);
        }
        END_FRAME();
        sp += STEP1_REGION_SIZE;
        sp = sp % STEP1_POST_FRAME_WORD;
      }
    }
    else {
      for (int j = 0; j < NY; j+=STEP1_UNROLL_LEN) {   
        prefetch_step1_frame_in0(ey, hz, i, j, NY, &sp);
        START_FRAME();
        #pragma GCC unroll(4)
        for (int u = 0; u < STEP1_UNROLL_LEN; u++) {
          int u0 = u;
          int u1 = STEP1_UNROLL_LEN+u;
          int u2 = 2*STEP1_UNROLL_LEN+u;
          DTYPE out = sp_ptr[sp + u0] - 0.5f * (sp_ptr[sp + u1] - sp_ptr[sp + u2]);
          STORE_NOACK(out, &ey[i * NY + j + u], 0);
        }
        END_FRAME();
        sp += STEP1_REGION_SIZE;
        sp = sp % STEP1_POST_FRAME_WORD;
      }
    }
  }
  #else
  for (int i = start; i < end; i++) {
    #pragma GCC unroll(4)
    for (int j = 0; j < NY; j++) {
      DTYPE out;
      if (i == 0) {
        out = fict[t];
      }
      else {
        out = ey[i * NY + j] - 0.5f * (hz[i * NY + j] - hz[(i-1) * NY + j]);
      }
      STORE_NOACK(out, &ey[i * NY + j], 0);
    }
  }
  #endif
  asm volatile("fence\n\t");
}

void fdtd_step2_manycore(DTYPE *ex, DTYPE *ey, DTYPE *hz, int t, int NX, int NY, int tid, int dim) {
  int start = ((tid + 0) * NX) / dim;
  int end   = ((tid + 1) * NX) / dim;

  #ifdef MANYCORE_PREFETCH
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);
  for (int i = start; i < end; i++) {
    for (int j = 1; j < NY; j+=STEP2_UNROLL_LEN) {
      prefetch_step2_frame(ex, hz, i, j, NY, &sp);

      START_FRAME();
      #pragma GCC unroll(4)
      for (int u = 0; u < STEP2_UNROLL_LEN; u++) {
        int u0 = u;
        int u1 = STEP2_UNROLL_LEN + u;
        if (j + u < NY) {
          DTYPE out = sp_ptr[sp + u0] - 
            0.5f * (sp_ptr[sp + u1 + 1] - sp_ptr[sp + u1]);
          STORE_NOACK(out, &ex[i * (NY+1) + j + u], 0);
        }
      }
      END_FRAME();
      sp += STEP2_REGION_SIZE;
      sp = sp % STEP2_POST_FRAME_WORD;
    }
  }
  #else
  for (int i = start; i < end; i++) {
    #pragma GCC unroll(4)
    for (int j = 1; j < NY; j++) {
      DTYPE out = ex[i * (NY+1) + j] - 0.5f * (hz[i * NY + j] - hz[i * NY + (j-1)]);
      STORE_NOACK(out, &ex[i * (NY+1) + j], 0); 
    }
  }
  #endif
  asm volatile("fence\n\t");
}

void fdtd_step3_manycore(DTYPE *ex, DTYPE *ey, DTYPE *hz, int t, int NX, int NY, int tid, int dim) {
  int start = ((tid + 0) * NX) / dim;
  int end   = ((tid + 1) * NX) / dim;

  #ifdef MANYCORE_PREFETCH
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);
  for (int i = start; i < end; i++) {
    for (int j = 0; j < NY; j+=STEP3_UNROLL_LEN) {
      prefetch_step3_frame(ex, ey, hz, i, j, NY, &sp);

      START_FRAME();
      #pragma GCC unroll(4)
      for (int u = 0; u < STEP3_UNROLL_LEN; u++) {
        int u0 = u;
        int u1 = STEP3_UNROLL_LEN + u;
        int u2 = 2*STEP3_UNROLL_LEN+1 + u;
        int u3 = 3*STEP3_UNROLL_LEN+1 + u;
        DTYPE out = sp_ptr[sp + u0] - 0.7f * 
          (sp_ptr[sp + u1+1] - sp_ptr[sp + u1] + sp_ptr[sp + u2] - sp_ptr[sp + u3]);
        STORE_NOACK(out, &hz[i * NY + j + u], 0); 
      }
      END_FRAME();
      sp += STEP3_REGION_SIZE;
      sp = sp % STEP3_POST_FRAME_WORD;
    }
  }
  #else
  for (int i = start; i < end; i++) {
    #pragma GCC unroll(4)
    for (int j = 0; j < NY; j++) {
      DTYPE out = hz[i * NY + j] - 0.7f * (ex[i * (NY+1) + (j+1)] - ex[i * (NY+1) + j] + ey[(i + 1) * NY + j] - ey[i * NY + j]);
      STORE_NOACK(out, &hz[i * NY + j], 0); 
    }
  }
  #endif

  asm volatile("fence\n\t");
}



void __attribute__((optimize("-fno-inline"))) fdtd(
    DTYPE *fict, DTYPE *ex, DTYPE *ey, DTYPE *hz, int NX, int NY, int tmax,
    int ptid, int vtid, int dim, int groupId, int numGroups,
    int mask, int used
  ) {

    for (int t = 0; t < tmax; t++) {
    #ifndef USE_VEC
      #ifdef MANYCORE_PREFETCH
      SET_PREFETCH_MASK(STEP1_NUM_REGIONS, STEP1_REGION_SIZE, &start_barrier);
      #else
      pthread_barrier_wait(&start_barrier);
      #endif
      fdtd_step1_manycore(fict, ex, ey, hz, t, NX, NY, ptid, dim);
      #ifdef MANYCORE_PREFETCH
      SET_PREFETCH_MASK(STEP2_NUM_REGIONS, STEP2_REGION_SIZE, &start_barrier);
      #else
      pthread_barrier_wait(&start_barrier);
      #endif
      fdtd_step2_manycore(ex, ey, hz, t, NX, NY, ptid, dim);
      #ifdef MANYCORE_PREFETCH
      SET_PREFETCH_MASK(STEP3_NUM_REGIONS, STEP3_REGION_SIZE, &start_barrier);
      #else
      pthread_barrier_wait(&start_barrier);
      #endif
      fdtd_step3_manycore(ex, ey, hz, t, NX, NY, ptid, dim);

    #else
      SET_PREFETCH_MASK(STEP1_NUM_REGIONS, STEP1_REGION_SIZE, &start_barrier);
      fdtd_step1_manycore(fict, ex, ey, hz, t, NX, NY, ptid, dim);
      // if (used)
        // tril_fdtd_step1(mask, fict, ex, ey, hz, t, NX, NY, ptid, groupId, numGroups, vtid);
      SET_PREFETCH_MASK(STEP2_NUM_REGIONS, STEP2_REGION_SIZE, &start_barrier);
      // fdtd_step2_manycore(ex, ey, hz, t, NX, NY, ptid, dim);
      if (used)
        tril_fdtd_step2(mask, ex, ey, hz, t, NX, NY, ptid, groupId, numGroups, vtid);
      SET_PREFETCH_MASK(STEP3_NUM_REGIONS, STEP3_REGION_SIZE, &start_barrier);
      // fdtd_step3_manycore(ex, ey, hz, t, NX, NY, ptid, dim);
      if (used)
        tril_fdtd_step3(mask, ex, ey, hz, t, NX, NY, ptid, groupId, numGroups, vtid);
    #endif
    }

}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *fict, DTYPE *ex, DTYPE *ey, DTYPE *hz, int NX, int NY, int tmax,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y) {
  
  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0) {
    stats_on();
  }

  // linearize tid and dim
  int ptid = ptid_x + ptid_y * pdim_x;
  int pdim = pdim_x * pdim_y;

  // split into physical and virtual tids + dim
  int vtid_x = 0;
  int vtid_y = 0;
  int vtid   = 0;
  int vdim_x = 0;
  int vdim_y = 0;
  int vdim   = 0;
  int unique_id = 0;
  int total_groups = 0;
  int used = 0;

  // group construction
  #ifdef USE_VEC

  #if VECTOR_LEN==4
  // template_info_t tinfo = init_template_4x4_2x2();
  template_info_t tinfo = init_template_debug();
  #elif VECTOR_LEN==16
  template_info_t tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  vtid = cinfo.vtid;
  vtid_x = cinfo.vtid_x;
  vtid_y = cinfo.vtid_y;
  vdim_x = cinfo.vdim_x;
  vdim_y = cinfo.vdim_y;
  unique_id = cinfo.unique_id;
  total_groups = cinfo.total_groups;
  used = cinfo.used;

  // printf("ptid %d(%d,%d) da %d vtid %d(%d,%d) dim %d(%d,%d) orig (%d,%d) used? %d\n", ptid, ptid_x, ptid_y, is_da, vtid, vtid_x, vtid_y, 4, vdim_x, vdim_y, orig_x, orig_y, used);

  #elif !defined(USE_VEC)

  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  used   = 1;

  #endif

  // linearize some fields
  vdim = vdim_x * vdim_y;

  // get behavior of each core
  #ifdef STEP1_NUM_REGIONS
  // setup up self prefetch
  #ifdef MANYCORE_PREFETCH
  core_config_info_t cinfo = manycore_template(ptid_x, ptid_y, pdim_x, pdim_y);
  int mask = getDebugMask(&cinfo);
  VECTOR_EPOCH(mask);
  #else
  int mask = getSIMDMask(&cinfo);
  #endif
  #else
  int mask = 0;
  #endif

  MOVE_STACK_ONTO_SCRATCHPAD();

  // compute fdtd
  fdtd(fict, ex, ey, hz, NX, NY, tmax, ptid, vtid, pdim, unique_id, total_groups, mask, used);

  // restore stack pointer
  RECOVER_DRAM_STACK();

}


// helper functions
Kern_Args *construct_args(DTYPE *fict, DTYPE *ex, DTYPE *ey, DTYPE *hz, int NX, int NY, int tmax,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->fict = fict;
  args->ex   = ex;
  args->ey   = ey;
  args->hz   = hz;
  args->NX    = NX;
  args->NY    = NY;
  args->tmax  = tmax;
  args->tid_x = tid_x;
  args->tid_y = tid_y;
  args->dim_x = dim_x;
  args->dim_y = dim_y;
  
  return args;
      
}

void *pthread_kernel(void *args) {
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early
  pthread_barrier_wait(&start_barrier);
  
  // call the spmd kernel
  Kern_Args *a = (Kern_Args*)args;
  
  kernel(a->fict, a->ex, a->ey, a->hz, a->NX, a->NY, a->tmax,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
