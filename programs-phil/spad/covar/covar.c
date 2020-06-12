#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "covar.h"
#include "spad.h"
#include "../../common/bind_defs.h"
#include "group_templates.h"

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
  int start = ((tid + 0) * M) / dim;
  int end   = ((tid + 1) * M) / dim;

  for (int j1 = start + 1; j1 < (end+1); j1++) {
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

/*-----------------------------------------------------------------------------------
 * Vector versions of the kernels.
 *---------------------------------------------------------------------------------*/

#ifdef USE_VEC
#endif


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

    #endif

}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *data, DTYPE *mean, DTYPE *symmat, int N, int M,
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }

  // linearize tid and dim
  int tid = tid_x + tid_y * dim_x;
  int dim = dim_x * dim_y;

  // split into physical and virtual tids + dim
  int ptid_x = tid_x;
  int ptid_y = tid_y;
  int ptid   = tid;
  int pdim_x = dim_x;
  int pdim_y = dim_y;
  int pdim   = dim;
  int vtid_x = 0;
  int vtid_y = 0;
  int vtid   = 0;
  int vdim_x = 0;
  int vdim_y = 0;
  int vdim   = 0;
  int orig_x = 0;
  int orig_y = 0;
  int is_da  = 0;
  int master_x = 0;
  int master_y = 0;
  int unique_id = 0;
  int total_groups = 0;
  int used = 0;

  // group construction
  #ifdef VECTOR_LEN

  #if VECTOR_LEN==4
  template_info_t tinfo = init_template_4x4_2x2();
  #elif VECTOR_LEN==16
  template_info_t tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  vtid = cinfo.vtid;
  vtid_x = cinfo.vtid_x;
  vtid_y = cinfo.vtid_y;
  vdim_x = cinfo.vdim_x;
  vdim_y = cinfo.vdim_y;
  orig_x = cinfo.orig_x;
  orig_y = cinfo.orig_y;
  is_da  = cinfo.is_scalar;
  master_x = cinfo.master_x;
  master_y = cinfo.master_y;
  unique_id = cinfo.unique_id;
  total_groups = cinfo.total_groups;
  used = cinfo.used;

  // printf("ptid %d(%d,%d) da %d vtid %d(%d,%d) dim %d(%d,%d) %d->%d used? %d\n", ptid, ptid_x, ptid_y, is_da, vtid, vtid_x, vtid_y, 4, vdim_x, vdim_y, start, end, used);

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
  int orig = orig_x + orig_y * dim_x;

  // get behavior of each core
  #ifdef USE_VEC
  int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
  SET_PREFETCH_MASK(NUM_FRAMES, FRAME_SIZE, &start_barrier);
  #else
  int mask = 0;
  #endif

  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later 
  unsigned long long *spTop = getSpTop(ptid);
  spTop -= 30;

  unsigned long long stackLoc;
  unsigned long long temp;
  #pragma GCC unroll(30)
  for(int i=0;i<30;i++){
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


  // compute covariance
  covar(data, mean, symmat, ptid, vtid, dim, N, M, unique_id, total_groups, mask, used);

  // restore stack pointer
  asm volatile (
    "addi sp, %[stackTop], 0\n\t" :: [stackTop] "r" (stackLoc)
  );

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
