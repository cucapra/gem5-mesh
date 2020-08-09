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

// compute each mean across each vector (single dimension)
void mean_manycore_baseline(DTYPE *mean, DTYPE *data, int N, int M, int tid, int dim) {
  int start = ((tid + 0) * M) / dim;
  int end   = ((tid + 1) * M) / dim;

  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);

  for (int j = start + 1; j < end + 1; j++) {
    DTYPE mean_j = 0.0f;

    #ifdef MANYCORE_PREFETCH
    for (int i = 1; i < (N+1); i+=MEAN_PREFETCH_LEN) {
      prefetch_mean_frame(data, i, j, &sp, M);

      FRAME_START();
      #pragma GCC unroll(16)
      for (int iin = 0; iin < MEAN_PREFETCH_LEN; iin++) {
        mean_j += sp_ptr[sp];
      }
      END_FRAME();

      sp += MEAN_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
    }
    #else
    for (int i = 1; i < (N+1); i++) {
      mean_j += data[i * (M+1) + j];
    }
    #endif
    mean_j /= (DTYPE)FLOAT_N;
    STORE_NOACK(mean_j, &mean[j], 0);
  }
  asm volatile("fence\n\t");
}

// subract mean from data to "center"
void center_manycore_baseline(DTYPE *mean, DTYPE *data, int N, int M, int tid, int dim) {
  // unlike gpu version only parallelize outer loop? find if N dimension big enough which prob is
  // or should we use same method as gpu to be fair?

  // I think just paralleize outer here? and maybe optimize gpu later?
  int start = ((tid + 0) * N) / dim;
  int end   = ((tid + 1) * N) / dim;

  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);

  for (int i = start + 1; i < end + 1; i++) {
    #ifdef MANYCORE_PREFETCH
    for (int j = 1; j < (M+1); j+=CENTER_PREFETCH_LEN) {
      prefetch_center_frame(data, mean, i, j, &sp, M);

      FRAME_START();
      // #pragma GCC unroll(16)
      // for (int jin = 0; jin < CENTER_PREFETCH_LEN; jin++) {

      // }
      DTYPE dat = data[i * (M+1) + j] - mean[j];
      END_FRAME();
      STORE_NOACK(dat, &data[i * (M+1) + j], 0);

      sp += CENTER_PREFETCH_LEN;
      sp = sp % POST_FRAME_WORD;
    }
    #else
    for (int j = 1; j < (M+1); j++) {
      DTYPE dat = data[i * (M+1) + j] - mean[j];
      STORE_NOACK(dat, &data[i * (M+1) + j], 0);
    }
    #endif
  }
  asm volatile("fence\n\t");
}

// compute the covariance matrix
void covar_manycore_baseline(DTYPE *symmat, DTYPE *data, int N, int M, int tid, int dim) {
  // if chunk then load balancing problem
  // opt for strided load balancing
  int start  = tid;
  int stride = dim;
  int end    = M;

  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);

  for (int j1 = start + 1; j1 < (end+1); j1+=stride) {
    for (int j2 = j1; j2 < (M+1); j2++) {
      DTYPE symmat_idx = 0.0f;

      #ifdef MANYCORE_PREFETCH
      for (int i = 1; i < (N+1); i+=COVAR_J1_PREFETCH_LEN) {
        prefetch_covar_frame(data, i, j1, j2, &sp, M);

        FRAME_START();
        #pragma GCC unroll(16)
        for (int iin = 0; iin < COVAR_J1_PREFETCH_LEN; iin++) {
          symmat_idx += sp_ptr[sp + 0] * sp_ptr[sp + 1];
        }
        END_FRAME();

        sp += COVAR_FRAME_SIZE;
        sp = sp % POST_FRAME_WORD;
      }
      #else
      for (int i = 1; i < (N+1); i++) {
        symmat_idx += data[i *(M+1) + j1] * data[i *(M+1) + j2];
      }
      #endif

      STORE_NOACK(symmat_idx, &symmat[j2 * (M+1) + j1], 0);
      STORE_NOACK(symmat_idx, &symmat[j1 * (M+1) + j2], 0);
      // symmat[j2 * (M+1) + j1] = symmat_idx;
      // symmat[j1 * (M+1) + j2] = symmat_idx;
    }
  }
  asm volatile("fence\n\t");
}

void __attribute__((optimize("-fno-inline"))) covar(
    DTYPE *data, DTYPE *mean, DTYPE *symmat,
    int ptid, int vtid, int dim, int N, int M, int groupId, int numGroups,
    int mask, int used
  ) {

    #ifndef USE_VEC
    #ifdef MANYCORE_PREFETCH
    SET_PREFETCH_MASK(NUM_MEAN_FRAMES, MEAN_FRAME_SIZE, &start_barrier);  
    #endif
    mean_manycore_baseline(mean, data, N, M, ptid, dim);
    #ifdef MANYCORE_PREFETCH
    SET_PREFETCH_MASK(NUM_CENTER_FRAMES, CENTER_FRAME_SIZE, &start_barrier);
    #else
    pthread_barrier_wait(&start_barrier);
    #endif
    center_manycore_baseline(mean, data, N, M, ptid, dim);
    #ifdef MANYCORE_PREFETCH
    SET_PREFETCH_MASK(NUM_COVAR_FRAMES, COVAR_FRAME_SIZE, &start_barrier);
    #else
    pthread_barrier_wait(&start_barrier);
    #endif
    covar_manycore_baseline(symmat, data, N, M, ptid, dim);
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
    DTYPE *data, DTYPE *mean, DTYPE *symmat, int N, int M,
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
  covar(data, mean, symmat, ptid, vtid, pdim, N, M, unique_id, total_groups, mask, used);

  // restore stack pointer
  RECOVER_DRAM_STACK();

}


// helper functions
Kern_Args *construct_args(DTYPE *data, DTYPE *mean, DTYPE *symmat, int N, int M,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->data = data;
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
  
  kernel(a->data, a->mean, a->symmat, a->N, a->M,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
