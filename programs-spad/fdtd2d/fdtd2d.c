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

  for (int i = start; i < end; i++) {
    for (int j = 0; j < NY; j++) {
      if (i == 0) {
        ey[i * NY + j] = fict[t];
      }
      else {
        ey[i * NY + j] = ey[i * NY + j] - 0.5f * (hz[i * NY + j] - hz[(i-1) * NY + j]);
      }
    }
  }
}

void fdtd_step2_manycore(DTYPE *ex, DTYPE *ey, DTYPE *hz, int t, int NX, int NY, int tid, int dim) {
  int start = ((tid + 0) * NX) / dim;
  int end   = ((tid + 1) * NX) / dim;

  for (int i = start; i < end; i++) {
    for (int j = 1; j < NY; j++) {
      ex[i * (NY+1) + j] = ex[i * (NY+1) + j] - 0.5f * (hz[i * NY + j] - hz[i * NY + (j-1)]);
    }
  }
}

void fdtd_step3_manycore(DTYPE *ex, DTYPE *ey, DTYPE *hz, int t, int NX, int NY, int tid, int dim) {
  int start = ((tid + 0) * NX) / dim;
  int end   = ((tid + 1) * NX) / dim;

  for (int i = start; i < end; i++) {
    for (int j = 0; j < NY; j++) {
      hz[i * NY + j] = hz[i * NY + j] - 0.7f * (ex[i * (NY+1) + (j+1)] - ex[i * (NY+1) + j] + ey[(i + 1) * NY + j] - ey[i * NY + j]);
    }
  }
}



void __attribute__((optimize("-fno-inline"))) fdtd(
    DTYPE *fict, DTYPE *ex, DTYPE *ey, DTYPE *hz, int NX, int NY, int tmax,
    int ptid, int vtid, int dim, int groupId, int numGroups,
    int mask, int used
  ) {

    for (int t = 0; t < tmax; t++) {
    #ifndef USE_VEC
      pthread_barrier_wait(&start_barrier);
      fdtd_step1_manycore(fict, ex, ey, hz, t, NX, NY, ptid, dim);
      pthread_barrier_wait(&start_barrier);
      fdtd_step2_manycore(ex, ey, hz, t, NX, NY, ptid, dim);
      pthread_barrier_wait(&start_barrier);
      fdtd_step3_manycore(ex, ey, hz, t, NX, NY, ptid, dim);

    #else
      SET_PREFETCH_MASK(STEP1_NUM_REGIONS, STEP1_REGION_SIZE, &start_barrier);
      // fdtd_step1_manycore(fict, ex, ey, hz, t, NX, NY, ptid, dim);
      if (used)
        tril_fdtd_step1(mask, fict, ex, ey, hz, t, NX, NY, ptid, groupId, numGroups, vtid);
      SET_PREFETCH_MASK(STEP2_NUM_REGIONS, STEP2_REGION_SIZE, &start_barrier);
      fdtd_step2_manycore(ex, ey, hz, t, NX, NY, ptid, dim);
      // if (used) // TODO on NY-1
        // tril_fdtd_step2(mask, ex, ey, hz, t, NX, NY, NY-1, ptid, groupId, numGroups, vtid);
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
  #ifdef VECTOR_LEN

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
  #ifdef USE_VEC
  int mask = getSIMDMask(&cinfo);
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
