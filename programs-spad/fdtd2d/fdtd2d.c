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
  Covariance
*/

/*-----------------------------------------------------------------------------------
 * Manycore. Using PolyBench GPU parallelization strategy. No scratchpad use
 *---------------------------------------------------------------------------------*/

// compute each mean across each vector (single dimension)
void mean_manycore_baseline(DTYPE *mean, DTYPE *data, int N, int M, int tid, int dim) {
  int start = ((tid + 0) * M) / dim;
  int end   = ((tid + 1) * M) / dim;

  for (int j = start + 1; j < end + 1; j++) {
    DTYPE mean_j = 0.0f;
    for (int i = 1; i < (N+1); i++) {
      mean_j += data[i * (M+1) + j];
    }
    mean_j /= (DTYPE)FLOAT_N;
    mean[j] = mean_j;
  }
}

// subract mean from data to "center"
void center_manycore_baseline(DTYPE *mean, DTYPE *data, int N, int M, int tid, int dim) {
  // unlike gpu version only parallelize outer loop? find if N dimension big enough which prob is
  // or should we use same method as gpu to be fair?

  // I think just paralleize outer here? and maybe optimize gpu later?
  int start = ((tid + 0) * N) / dim;
  int end   = ((tid + 1) * N) / dim;

  for (int i = start + 1; i < end + 1; i++) {
    for (int j = 1; j < (M+1); j++) {
      data[i * (M+1) + j] -= mean[j];	
    }
  }
}

// compute the covariance matrix
void covar_manycore_baseline(DTYPE *symmat, DTYPE *data, int N, int M, int tid, int dim) {
  // if chunk then load balancing problem
  // opt for strided load balancing
  int start  = tid;
  int stride = dim;
  int end    = M;

  for (int j1 = start + 1; j1 < (end+1); j1+=stride) {
    for (int j2 = j1; j2 < (M+1); j2++) {
      DTYPE symmat_idx = 0.0f;
      for (int i = 1; i < (N+1); i++) {
        symmat_idx += data[i *(M+1) + j1] * data[i *(M+1) + j2];
      }
      symmat[j2 * (M+1) + j1] = symmat_idx;
      symmat[j1 * (M+1) + j2] = symmat_idx;
    }
  }
}

void __attribute__((optimize("-fno-inline"))) covar(
    DTYPE *data, DTYPE *mean, DTYPE *symmat,
    int ptid, int vtid, int dim, int N, int M, int groupId, int numGroups,
    int mask, int used
  ) {

    #ifndef USE_VEC
    mean_manycore_baseline(mean, data, N, M, ptid, dim);
    pthread_barrier_wait(&start_barrier);
    center_manycore_baseline(mean, data, N, M, ptid, dim);
    pthread_barrier_wait(&start_barrier);
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
  #ifdef VECTOR_LEN

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
  #ifdef USE_VEC
  int mask = getSIMDMask(&cinfo);
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
