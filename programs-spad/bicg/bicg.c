#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "bicg.h"
#include "spad.h"
#include "../../common/bind_defs.h"
#include "group_templates.h"

/*
  big c
*/

/*-----------------------------------------------------------------------------------
 * Manycore. Using PolyBench GPU parallelization strategy. No scratchpad use
 *---------------------------------------------------------------------------------*/

// compute s by parallezing the outerloop around reduction (reductions done within a single core)
void compute_s_manycore_baseline(DTYPE *a, DTYPE *r, DTYPE *s, int NX, int NY, int tid, int dim) {

  // paralleize outer loop y
  int start = ((tid + 0) * NY) / dim;
  int end   = ((tid + 1) * NY) / dim;

  for (int j = start; j < end; j++) {
    // s[j] = 0.0f;
    DTYPE s_local = 0.0f;
    for (int i = 0; i < NX; i++) {
      // s[j] += a[i * NY + j] * r[i];
      s_local += a[i * NY + j] * r[i];
    }
    s[j] = s_local;
    // STORE_NOACK(s_local, &s[j], 0);
  }

  // asm volatile("fence\n\t");
}

// compute q by paralleization outerloop around reduction (reduction done within a single core)
// note loops are in the opposite order as in the previous kernel to allow for this strategy
void compute_q_manycore_baseline(DTYPE *a, DTYPE *p, DTYPE *q, int NX, int NY, int tid, int dim) {

  // paralleize outer loop x
  int start = ((tid + 0) * NX) / dim;
  int end   = ((tid + 1) * NX) / dim;

  for (int i = start; i < end; i++) {
    // q[i] = 0.0f;
    DTYPE q_local = 0.0f;
    for (int j = 0; j < NY; j++) {
      // q[i] += a[i * NY + j] * p[j];
      q_local += a[i * NY + j] * p[j];
    }
    q[i] = q_local;
    // STORE_NOACK(q_local, &q[i], 0);
  }

  // asm volatile("fence\n\t");
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

// prefetch a and r
inline void prefetch_s_frame(DTYPE *a, DTYPE *r, int i, int j, int *sp, int NY) {
  // don't think need prefetch_R here?
  VPREFETCH_L(*sp, &a[i * NY + j], 0, S_PREFETCH_LEN, HORIZONTAL);
  // VPREFETCH_R(*sp, &a[i * NY + j], 0, S_PREFETCH_LEN, HORIZONTAL);
  // printf("horiz pf i %d j %d idx %d sp %d val %f %f %f %f\n", i, j, i * NY + j, *sp, a[i* NY + j], a[i*NY+j+1], a[i*NY+j+2], a[i*NY+j+3]);
  *sp = *sp + 1;

  // TODO potentially some reuse here b/c fetch the same thing for everyone
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp, &r[i], core, Q_PREFETCH_LEN, VERTICAL);
    // VPREFETCH_R(*sp, &r[i], core, Q_PREFETCH_LEN, VERTICAL);
  }

  *sp = *sp + Q_PREFETCH_LEN;

  if (*sp == POST_FRAME_WORD) *sp = 0;
}

void  __attribute__((optimize("-fno-reorder-blocks")))
 compute_s_vector_opt(DTYPE *a, DTYPE *r, DTYPE *s, int NX, int NY, int ptid, int groupId, int numGroups, int vtid, int mask) {

  // chunk over vector gorups
  int start = ((groupId + 0) * NY) / numGroups;
  int end   = ((groupId + 1) * NY) / numGroups;

  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);

  // prevents code from being reordered :|
  volatile int ohjeez = 1;
  if (ohjeez) {

  // if (ptid == 0) {
  // goto vector mode
  VECTOR_EPOCH(mask);
  // VECTOR_EPOCH((1 << FET_XORIGIN_SHAMT) | (0 << FET_YORIGIN_SHAMT) | (2 << FET_XLEN_SHAMT) | (2 << FET_YLEN_SHAMT) | (1 << FET_DAE_SHAMT));
  
  // issue header block
  ISSUE_VINST(fable0);

  int sp = 0;
  
  // do initial prefetching for a small amount of first row
  for (int i = 0; i < INIT_FRAMES*Q_PREFETCH_LEN; i+=Q_PREFETCH_LEN) {
    prefetch_s_frame(a, r, i, start, &sp, NY);
  }

  // do modified first row that has already been prefetch
  for (int i = INIT_FRAMES*Q_PREFETCH_LEN; i < NX; i+=Q_PREFETCH_LEN) {
    prefetch_s_frame(a, r, i, start, &sp, NY);
    ISSUE_VINST(fable1);
  }

  // steady state
  for (int j = start + VECTOR_LEN; j < end; j+=VECTOR_LEN) {
    for (int i = 0; i < NX; i+=Q_PREFETCH_LEN) {
      prefetch_s_frame(a, r, i, j, &sp, NY);
      ISSUE_VINST(fable1);
    }
  }

  // draining. do the last vissue corresponding to the initial round of prefetch
  for (int i = NX - INIT_FRAMES*Q_PREFETCH_LEN; i < NX; i+=Q_PREFETCH_LEN) {
    ISSUE_VINST(fable1);
  }

  // devec with unique tag
  DEVEC(devec_0);

  // }
  // else {
  //   int sp = 0;
  //   DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  //   for (int j = start + vtid; j < end; j+=VECTOR_LEN) {
  //     DTYPE s_local = 0.0f;
  //     for (int i = 0; i < NX; i++) {
  //       FRAME_START(FRAME_SIZE);
  //       // s_local += a[i * NY + j] * r[i];
  //       // printf("vtid %d sp %d a %f ?= %f r %f ?= %f\n", vtid, sp, a[i*NY+j], sp_ptr[sp + 0], r[i], sp_ptr[sp + 1]);
  //       s_local += sp_ptr[sp + 0] * sp_ptr[sp + 1];
  //       sp+=2;
  //       if (sp == POST_FRAME_WORD) sp = 0;
  //       REMEM(FRAME_SIZE);
  //     }
  //     s[j] = s_local;
  //   }
  // }
  // VECTOR_EPOCH(0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  return;
  }

  // vector engine code

  // declarations
  int i, j;
  int sp;
  DTYPE *sp_ptr;
  DTYPE s_local;

  // header
  fable0:
    i = 0;
    j = start + vtid;
    s_local = 0.0f;
    sp = 0;
    sp_ptr = (DTYPE*)getSpAddr(ptid, 0);

  // body
  fable1:
    FRAME_START(FRAME_SIZE);
    // s_local += a[i * NY + j] * r[i];
    s_local += sp_ptr[sp + 0] * sp_ptr[sp + 1];
    i++;
    // do loop check here, to take load off scalar core?
    // does reduce vector core utilization
    if (i == NX) {
      STORE_NOACK(s_local, &s[j], 0);
      i = 0;
      j+=VECTOR_LEN;
      s_local = 0.0f;
    }
    sp+=2;
    if (sp == POST_FRAME_WORD) sp = 0;
    REMEM(FRAME_SIZE);
    asm volatile goto("j %l[fable1]\n\t"::::fable1);
}

// prefetch a and p
inline void prefetch_q_frame(DTYPE *a, DTYPE *p, int i, int j, int *sp, int NY) {
  // don't think need prefetch R here?
  for (int core = 0; core < VECTOR_LEN; core++) {
    int icore = i + core;
    VPREFETCH_L(*sp, &a[icore * NY + j], core, Q_PREFETCH_LEN, VERTICAL);
    VPREFETCH_R(*sp, &a[icore * NY + j], core, Q_PREFETCH_LEN, VERTICAL);
  }

  *sp = *sp + Q_PREFETCH_LEN;

  // TODO potentially some reuse here b/c fetch the same thing for everyone
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp, &p[j], core, Q_PREFETCH_LEN, VERTICAL);
    VPREFETCH_R(*sp, &p[j], core, Q_PREFETCH_LEN, VERTICAL);
  }

  *sp = *sp + Q_PREFETCH_LEN;

  if (*sp == POST_FRAME_WORD) *sp = 0;
}

void  __attribute__((optimize("-fno-reorder-blocks")))
 compute_q_vector_opt(DTYPE *a, DTYPE *p, DTYPE *q, int NX, int NY, int ptid, int groupId, int numGroups, int vtid, int mask) {

  // chunk over vector gorups
  int start = ((groupId + 0) * NX) / numGroups;
  int end   = ((groupId + 1) * NX) / numGroups;

  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);

  // prevents code from being reordered :|
  volatile int ohjeez = 1;
  if (ohjeez) {

  // goto vector mode
  VECTOR_EPOCH(mask);
  
  // issue header block
  ISSUE_VINST(fable0);

  // TODO seems like can use c functors to implement this pattern
  // just need to give functions for prefetch and vissue?

  int sp = 0;

  // this kernel needs to do vertical loads due to access pattern of A
  // currently doing a one wide load. want to increase size, but then also have
  // to unroll vector code

  // TODO merge INIT_FRAMES*Q_PREFEATCH_LEN

  // do initial prefetching for a small amount of first row
  for (int j = 0; j < INIT_FRAMES*Q_PREFETCH_LEN; j+=Q_PREFETCH_LEN) {
    prefetch_q_frame(a, p, start, j, &sp, NY);
  }

  // do modified first row that has already been prefetch
  for (int j = INIT_FRAMES*Q_PREFETCH_LEN; j < NY; j+=Q_PREFETCH_LEN) {
    prefetch_q_frame(a, p, start, j, &sp, NY);
    ISSUE_VINST(fable1);
  }

  // steady state
  for (int i = start + VECTOR_LEN; i < end; i+=VECTOR_LEN) {
    for (int j = 0; j < NY; j+=Q_PREFETCH_LEN) {
      prefetch_q_frame(a, p, i, j, &sp, NY);
      ISSUE_VINST(fable1);
    }
  }

  // draining. do the last vissue corresponding to the initial round of prefetch
  for (int j = NY - INIT_FRAMES*Q_PREFETCH_LEN; j < NY; j+=Q_PREFETCH_LEN) {
    ISSUE_VINST(fable1);
  }

  // devec with unique tag
  DEVEC(devec_0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  return;
  }

  // vector engine code

  // declarations
  int i, j;
  int sp;
  DTYPE *sp_ptr;
  DTYPE q_local;

  // header
  fable0:
    i = start + vtid;
    j = 0;
    q_local = 0.0f;
    sp = 0;
    sp_ptr = (DTYPE*)getSpAddr(ptid, 0);

  // body
  fable1:
    FRAME_START(FRAME_SIZE);
    // q_local += a[i * NY + j] * p[j];
    q_local += sp_ptr[sp + 0] * sp_ptr[sp + 1];
    j++;
    // do loop check here, to take load off scalar core?
    // does reduce vector core utilization
    if (j == NY) {
      STORE_NOACK(q_local, &q[i], 0);
      i += VECTOR_LEN;
      j = 0;
      q_local = 0.0f;
    }
    sp += 2;
    if (sp == POST_FRAME_WORD) sp = 0;
    REMEM(FRAME_SIZE);
    asm volatile goto("j %l[fable1]\n\t"::::fable1);
}

#endif


void __attribute__((optimize("-fno-inline"))) bicg(
    DTYPE *a, DTYPE *r, DTYPE *p, DTYPE *s, DTYPE *q, 
    int ptid, int vtid, int dim, int NX, int NY, int groupId, int numGroups,
    int mask, int used
  ) {

    #ifndef USE_VEC
    compute_s_manycore_baseline(a, r, s, NX, NY, ptid, dim);
    // don't need a barrier in between b/c s and q can be compute independently
    compute_q_manycore_baseline(a, p, q, NX, NY, ptid, dim);
    #else
    // if (groupId != 0) return;

    if (used)
      compute_s_vector_opt(a, r, s, NX, NY, ptid, groupId, numGroups, vtid, mask);

    // reset frames. technically don't have to do this is pass the last sp ptr between these two
    SET_PREFETCH_MASK(NUM_FRAMES, FRAME_SIZE, &start_barrier);

    if (used)
      compute_q_vector_opt(a, p, q, NX, NY, ptid, groupId, numGroups, vtid, mask);
    #endif

}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *r, DTYPE *p, DTYPE *s, DTYPE *q,  int NX, int NY,
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
  // if (ptid == 0)
  //   printf("initial pf mask\n");
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


  // gramschmidt
  bicg(a, r, p, s, q, ptid, vtid, dim, NX, NY, unique_id, total_groups, mask, used);

  // restore stack pointer
  asm volatile (
    "addi sp, %[stackTop], 0\n\t" :: [stackTop] "r" (stackLoc)
  );

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *r, DTYPE *p, DTYPE *s, DTYPE *q, int NX, int NY,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->r = r;
  args->p = p;
  args->s = s;
  args->q = q;
  args->NX = NX;
  args->NY  = NY;
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
  
  kernel(a->a, a->r, a->p, a->s, a->q, a->NX, a->NY,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
