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

int roundUp(int numToRound, int multiple) {
  if (multiple == 0) {
    return numToRound;
  }

  int remainder = abs(numToRound) % multiple;
  if (remainder == 0) {
    return numToRound;
  }

  if (numToRound < 0) {
    return -(abs(numToRound) - remainder);
  }
  else {
    return numToRound + multiple - remainder;
  }
}

inline void prefetch_mean_frame(DTYPE *data, int i, int j, int *sp, int M) {
  VPREFETCH_L(*sp, &data[i * (M+1) + j], 0, MEAN_PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_R(*sp, &data[i * (M+1) + j], 0, MEAN_PREFETCH_LEN, HORIZONTAL);

  *sp = *sp + 1;
  if (*sp == POST_FRAME_WORD) *sp = 0;
}

// compute each mean across each vector (single dimension)
void mean_vector_opt(DTYPE *mean, DTYPE *data, int N, int M, 
    int ptid, int groupId, int numGroups, int vtid, int mask) {
  int start = ((groupId + 0) * M) / numGroups;
  int end   = ((groupId + 1) * M) / numGroups;

  // make it a factor of vector group mapping size
  start = 1 + roundUp(start, VECTOR_LEN);
  end   = 1 + roundUp(end  , VECTOR_LEN);

  int sp  = 0;


  VECTOR_EPOCH(mask);

  // printf("ptid %d range %d->%d enter\n", ptid, start, end);

  if (ptid == 0) {
  
  // ISSUE_VINST()

  // initial round
  for (int i = 1; i < 1 + INIT_MEAN_OFFSET; i++) {
    prefetch_mean_frame(data, i, start, &sp, M);
  }

  // first row
  for (int i = 1 + INIT_MEAN_OFFSET; i < (N+1); i++) {
    prefetch_mean_frame(data, i, start, &sp, M);
    // ISSUE_VINST()
  }

  // steady state
  for (int j = start + VECTOR_LEN; j < end; j+=VECTOR_LEN) {
    for (int i = 1; i < (N+1); i++) {
      prefetch_mean_frame(data, i, j, &sp, M);
      // ISSUE_VINST()
    }
  }

  // cooldown
  for (int i = N - INIT_MEAN_OFFSET; i < (N+1); i++) {
    // ISSUE_VINST()
  }

  }
  else {
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  for (int j = start + vtid; j < end; j+=VECTOR_LEN) {
    DTYPE mean_j = 0.0f;
    for (int i = 1; i < (N+1); i++) {
      FRAME_START(MEAN_FRAME_SIZE);
      mean_j += sp_ptr[sp];
      REMEM(MEAN_FRAME_SIZE);
      sp++;
      if (sp == POST_FRAME_WORD) sp = 0;
    }
    mean_j /= (DTYPE)FLOAT_N;
    mean[j] = mean_j;
  }

  }

  // for (int j = start + 1; j < end + 1; j++) {
  //   DTYPE mean_j = 0.0f;
  //   for (int i = 1; i < (N+1); i++) {
  //     mean_j += data[i * (M+1) + j];
  //   }
  //   mean_j /= (DTYPE)FLOAT_N;
  //   mean[j] = mean_j;
  // }
}

inline void prefetch_center_frame(DTYPE *data, DTYPE *mean, int i, int j, int *sp, int M) {
  // fetch data
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp, &data[i * (M+1) + j], 0, CENTER_PREFETCH_LEN, VERTICAL);
  }

  *sp = *sp + CENTER_PREFETCH_LEN;

  // TODO should do more than 1 here
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp, &mean[j], 0, CENTER_PREFETCH_LEN, VERTICAL);
  }
  *sp = *sp + CENTER_PREFETCH_LEN;
  if (*sp == POST_FRAME_WORD) *sp = 0;
}

// subract mean from data to "center"
void center_vector_opt(DTYPE *mean, DTYPE *data, int N, int M, int tid, int dim) {
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

inline void prefetch_covar_frame(DTYPE *data, int i, int j1, int j2, int *sp, int M) {
  // everyone in groups gets the same j1. could share and/or do vertical
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp, &data[i * (M+1) + j1], core, COVAR_J1_PREFETCH_LEN, VERTICAL);
  }
  *sp = *sp + COVAR_J1_PREFETCH_LEN;

  VPREFETCH_L(*sp, &data[i * (M+1) + j2], 0, COVAR_J2_PREFETCH_LEN, HORIZONTAL);
  *sp = *sp + 1;

  if (*sp == POST_FRAME_WORD) *sp = 0;
}

// compute the covariance matrix
void covar_vector_opt(DTYPE *symmat, DTYPE *data, int N, int M, int tid, int dim) {
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
    SET_PREFETCH_MASK(NUM_MEAN_FRAMES, MEAN_FRAME_SIZE, &start_barrier);
    if (used)
      mean_vector_opt(mean, data, N, M, ptid, groupId, numGroups, vtid, mask);
    SET_PREFETCH_MASK(NUM_CENTER_FRAMES, CENTER_FRAME_SIZE, &start_barrier);
    center_manycore_baseline(mean, data, N, M, ptid, dim);
    pthread_barrier_wait(&start_barrier);
    covar_manycore_baseline(symmat, data, N, M, ptid, dim); 
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
  orig_x = cinfo.orig_x;
  orig_y = cinfo.orig_y;
  is_da  = cinfo.is_scalar;
  master_x = cinfo.master_x;
  master_y = cinfo.master_y;
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
  int orig = orig_x + orig_y * dim_x;

  // get behavior of each core
  #ifdef USE_VEC
  // int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
  int mask = getDebugMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
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
