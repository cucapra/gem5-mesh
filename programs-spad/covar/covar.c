#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "covar.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "covar_kernel.h"

/*
  Covariance
*/

/*-----------------------------------------------------------------------------------
 * Manycore. Using PolyBench GPU parallelization strategy. No scratchpad use
 *---------------------------------------------------------------------------------*/

void transpose_manycore(DTYPE *a, int a_row, int a_col, DTYPE *aT, int ptid, int pdim){

  int start = ((ptid + 0) * a_col) / pdim;
  int end = ((ptid + 1) * a_col) / pdim;

  for(int i=start; i<end; i++){
    for(int j=0; j<a_row; j++){
      aT[i*a_row+j] = a[j*a_col+i];
    }
  }
}


// compute each mean across each vector (single dimension)
void mean_manycore(DTYPE *mean, DTYPE *data, int N, int M, int tid, int dim) {
  int start = ((tid + 0) * N) / dim;
  int end   = ((tid + 1) * N) / dim;

  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);

  for (int i = start; i < end; i++) { // TODO remove +1, keep for now for eq
    DTYPE mean_i = 0.0f;

    #ifdef MANYCORE_PREFETCH
    for (int j = 0; j < M; j+=MEAN_UNROLL_LEN) {
      prefetch_mean_frame(data, i, j, &sp, M);

      START_FRAME();
      #pragma GCC unroll(8)
      for (int u = 0; u < MEAN_UNROLL_LEN; u++) {
        mean_i += sp_ptr[sp + u];
      }
      END_FRAME();

      sp += MEAN_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
    }
    #else

    // compute mean
    #pragma GCC unroll(8)
    for (int j = 0; j < M; j++) {
      mean_i += data[i * N + j];
    }
    #endif
    mean_i /= (DTYPE)FLOAT_N;

    // TODO dont need this
    STORE_NOACK(mean_i, &mean[i], 0);

    for (int j = 0; j < M; j+=MEAN_UNROLL_LEN) {
      prefetch_mean_frame(data, i, j, &sp, M);

      START_FRAME();
      #pragma GCC unroll(8)
      for (int u = 0; u < MEAN_UNROLL_LEN; u++) {
        DTYPE dat = sp_ptr[sp + u] - mean_i;
        STORE_NOACK(dat, &data[i * N + j], 0);
      }
      END_FRAME();

      sp += MEAN_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
    }
  }
  asm volatile("fence\n\t");
}

// compute the covariance matrix
void covar_manycore(DTYPE *symmat, DTYPE *data, int N, int M, int tid, int dim) {
  // if chunk then load balancing problem
  // opt for strided load balancing
  int start  = tid;
  int stride = dim;

  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);

  for (int i1 = start; i1 < N; i1+=stride) {
    for (int i2 = i1; i2 < N; i2++) {
      DTYPE symmat_idx = 0.0f;

      #ifdef MANYCORE_PREFETCH
      for (int j = 0; j < M; j+=COVAR_UNROLL_LEN) {
        prefetch_covar_frame(data, i1, i2, j, &sp, M);

        START_FRAME();
        #pragma GCC unroll(8)
        for (int u = 0; u < COVAR_UNROLL_LEN; u++) {
          symmat_idx += sp_ptr[sp + u] * sp_ptr[sp + COVAR_UNROLL_LEN + u];
        }
        END_FRAME();
        sp += COVAR_FRAME_SIZE;
        sp = sp % POST_FRAME_WORD;
      }
      #else
      #pragma GCC unroll(8)
      for (int j = 0; j < M; j++) {
        symmat_idx += data[i1 * N + j] * data[i2 * N + j];
      }
      #endif

      STORE_NOACK(symmat_idx, &symmat[i2 * N + i1], 0);
      STORE_NOACK(symmat_idx, &symmat[i1 * N + i2], 0);
      // symmat[j2 * (M+1) + j1] = symmat_idx;
      // symmat[j1 * (M+1) + j2] = symmat_idx;
    }
  }
  asm volatile("fence\n\t");
}

void __attribute__((optimize("-fno-inline"))) covar(
    DTYPE *data, DTYPE *dataT, DTYPE *mean, DTYPE *symmat,
    int ptid, int vtid, int dim, int N, int M, int groupId, int numGroups,
    int mask, int used
  ) {

    transpose_manycore(data, M, N, dataT, ptid, dim);

    #ifndef USE_VEC
    #ifdef MANYCORE_PREFETCH
    SET_PREFETCH_MASK(NUM_MEAN_FRAMES, MEAN_FRAME_SIZE, &start_barrier);
    #else
    pthread_barrier_wait(&start_barrier);
    #endif
    mean_manycore(mean, dataT, N, M, ptid, dim);
    #ifdef MANYCORE_PREFETCH
    SET_PREFETCH_MASK(NUM_COVAR_FRAMES, COVAR_FRAME_SIZE, &start_barrier);
    #else
    pthread_barrier_wait(&start_barrier);
    #endif
    covar_manycore(symmat, dataT, N, M, ptid, dim);
    #else
    SET_PREFETCH_MASK(NUM_MEAN_FRAMES, MEAN_FRAME_SIZE, &start_barrier);
    if (used)
      tril_mean(mask, mean, data, N, M, ptid, groupId, numGroups, vtid);
    
    SET_PREFETCH_MASK(NUM_CENTER_FRAMES, CENTER_FRAME_SIZE, &start_barrier);
    if (used)
      tril_center(mask, mean, data, N, M, ptid, groupId, numGroups, vtid);
    
    SET_PREFETCH_MASK(NUM_COVAR_FRAMES, COVAR_FRAME_SIZE, &start_barrier);
    if (used)
      tril_covar(mask, symmat, data, N, M, ptid, groupId, numGroups, vtid);
    #endif

}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *data, DTYPE *dataT, DTYPE *mean, DTYPE *symmat, int N, int M,
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
  template_info_t tinfo = init_template_4x4_2x2();
  // template_info_t tinfo = init_template_debug();
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
  #ifdef MEAN_FRAME_SIZE
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

  // compute covariance
  covar(data, dataT, mean, symmat, ptid, vtid, pdim, N, M, unique_id, total_groups, mask, used);

  // restore stack pointer
  RECOVER_DRAM_STACK();

}


// helper functions
Kern_Args *construct_args(DTYPE *data, DTYPE *dataT, DTYPE *mean, DTYPE *symmat, int N, int M,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->data = data;
  args->dataT = dataT;
  args->mean = mean;
  args->symmat = symmat;
  args->N = N;
  args->M = M;
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
  
  kernel(a->data, a->dataT, a->mean, a->symmat, a->N, a->M,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
