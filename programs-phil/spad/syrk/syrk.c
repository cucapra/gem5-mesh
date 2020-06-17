#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "syrk.h"
#include "spad.h"
#include "../../common/bind_defs.h"
#include "group_templates.h"

/*
  syrk kernel
*/

/*-----------------------------------------------------------------------------------
 * Manycore. Using PolyBench GPU parallelization strategy. No scratchpad use
 *---------------------------------------------------------------------------------*/

// compute s by parallezing the outerloop around reduction (reductions done within a single core)
void syrk_manycore_baseline(DTYPE *a, DTYPE *c, int N, int M, int tid, int dim) {
  // could parallize over two dimensions. thats what gpu version does
  // just do 1d so easier
  int start = ((tid + 0) * N) / dim;
  int end   = ((tid + 1) * N) / dim;
  
  for (int i = start; i < end; i++) {
    for (int j = 0; j < M; j++) {
      c[i * N + j] *= beta;

      for (int k = 0; k < M; k++) {
        c[i * N + j] += alpha * a[i * M + k] * a[j * M + k];
      }
    }
  }
}

/*-----------------------------------------------------------------------------------
 * Vector versions of the kernels.
 *---------------------------------------------------------------------------------*/

#ifdef USE_VEC
// helper for mapping to vector groups.
// doesn't handle odd numbers would need to split off into manycore kernel to handle that
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

// prefetch c
// pad out to the frame size (1->2 currently)
// maybe don't have to prefetch this
inline void prefetch_outer_frame(DTYPE *c, int i, int j, int *sp, int N) {
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp + 0, &c[i * N + j], core, OUTER_PREFETCH_LEN, VERTICAL);

    // pad out
    VPREFETCH_L(*sp + 1, &c[i * N + j], core, OUTER_PREFETCH_LEN, VERTICAL);
  }

  *sp = *sp + 2*OUTER_PREFETCH_LEN;
  if (*sp == POST_FRAME_WORD) *sp = 0;
}

// prefetch a
inline void prefetch_inner_frame(DTYPE *a, int i, int j, int k, int *sp, int M) {
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp + 0, &a[i * M + k], core, INNER_PREFETCH_LEN, VERTICAL);

    VPREFETCH_L(*sp + 1, &a[j * M + k], core, INNER_PREFETCH_LEN, VERTICAL);
  }

  *sp = *sp + 2*INNER_PREFETCH_LEN;
  if (*sp == POST_FRAME_WORD) *sp = 0;
}

// TODO there are def oppurtuniteis to parallize inner loop instead of outer loop to get more horizontal prefetching
//
// for (int i = start + vtid; i < end; i+=VECTOR_LEN) {
//  for (int j = 0; j < M; j++) {
//    c[i * N + j]
//  }
// }
//
// should be 
//
// for (int i = start; i < end; i++{
//  for (int j = vtid; j < M; j+=VECTOR_LEN) {
//    c[i * N + j]
//  }
// }
//
// then can use horizontal prefetching

void  __attribute__((optimize("-fno-reorder-blocks")))
    syrk_vector_opt(int mask, DTYPE *a, DTYPE *c, int N, int M, 
                  int ptid, int groupId, int numGroups, int vtid) {


  // chunk over vector gorups
  int start = ((groupId + 0) * N) / numGroups;
  int end   = ((groupId + 1) * N) / numGroups;

  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);

  VECTOR_EPOCH(mask);

  int sp = 0;

  if (ptid == 0) {
  // ISSUE_VINST(fable0);
    
  // get ahead
  prefetch_outer_frame(c, start, 0, &sp, N);
  for (int k = 0; k < INIT_OFFSET; k+=INNER_PREFETCH_LEN) {
    prefetch_inner_frame(a, start, 0, k, &sp, M);
  }

  // do first inner loop
  for (int k = INIT_OFFSET; k < M; k+=INNER_PREFETCH_LEN) {
    prefetch_inner_frame(a, start, 0, k, &sp, M);
    // ISSUE_VINST(fable1);
  }

  // steady state
  for (int i = start; i < end; i++) {
    int startJ = 0;
    if (i == start) startJ += VECTOR_LEN;
    for (int j = startJ; j < M; j+=VECTOR_LEN) {
      prefetch_outer_frame(c, i, j, &sp, N);

      for (int k = 0; k < M; k+=INNER_PREFETCH_LEN) {
        prefetch_inner_frame(a, i, j, k, &sp, M);
        // ISSUE_VINST(fable1);
      }
    }
  }

  // draining. do the last vissue corresponding to the initial round of prefetch
  for (int k = N - INIT_OFFSET; k < N; k+=INNER_PREFETCH_LEN) {
    // ISSUE_VINST(fable1);
  }

  // // devec with unique tag
  // DEVEC(devec_0);
  }
  else {
  for (int i = start + vtid; i < end; i+=VECTOR_LEN) {
    for (int j = 0; j < M; j++) {
      FRAME_START(OUTER_FRAME_SIZE);
      c[i * N + j] *= beta;
      REMEM(OUTER_FRAME_SIZE);

      for (int k = 0; k < M; k++) {
        FRAME_START(INNER_FRAME_SIZE);
        c[i * N + j] += alpha * a[i * M + k] * a[j * M + k];
        REMEM(INNER_FRAME_SIZE);
      }
    }
  }
  }


  // can we change where paralleization is to get more horiz prefetch?
  // for (int i = start; i < end; i++) {
  //   for (int j = 0; j < M; j++) {
  //     c[i * N + j] *= beta;

  //     for (int k = 0; k < M; k++) {
  //       c[i * N + j] += alpha * a[i * M + k] * a[j * M + k];
  //     }
  //   }
  // }

}
#endif


void __attribute__((optimize("-fno-inline"))) syrk(
    DTYPE *a, DTYPE *c,
    int ptid, int vtid, int dim, int N, int M, int groupId, int numGroups,
    int mask, int used
  ) {

    #ifndef USE_VEC
    syrk_manycore_baseline(a, c, N, M, ptid, dim);
    #else
    if (used)
      syrk_vector_opt(mask, a, c, N, M, ptid, groupId, numGroups, vtid);
    #endif

}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *c, int N, int M,
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
  // int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
  int mask = getDebugMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
  SET_PREFETCH_MASK(NUM_FRAMES, INNER_FRAME_SIZE, &start_barrier);
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


  // do the kernel
  syrk(a, c, ptid, vtid, dim, N, M, unique_id, total_groups, mask, used);

  // restore stack pointer
  asm volatile (
    "addi sp, %[stackTop], 0\n\t" :: [stackTop] "r" (stackLoc)
  );

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *c, int N, int M,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->c = c;
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
  
  kernel(a->a, a->c, a->N, a->M,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
