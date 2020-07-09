#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "gramschmidt.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "gram_kernel.h"

/*
  Gram-Schmidt Orthonormalization

  Data: N M'd vectors as a flat array

  Each orthonormalization step is dependent on the last so can't parallezie across it and need seperate rounds

  Successively 
  u_k+1 = v_k+1 - proj|u_k(v) = v_k+1 - <v_k+1, u_k> / <u_k, u_k> * u_k
  Note each step does all proj|u_x. So keep around <u_k, u_k>

  Where u are orthormal vectors and v are non-orthonormal vectors

  a has vectors. In place modification of them
  r has magnitude of orthonormalized vectors. This is reused in future iterations. Not parallelized in polybench
  q is the normlized vector. Parallelize computation of each element. Reused in future iterations

  Each step (same number can happen in parallel)
  0) Sequential over each vector to orthonormalize
  1) Sequential: Get magnitude of next orthonormalized vector
  2) Parallel over vec dim: Normalize next orthonromalized vector
  3) Parallel over num prev vecs: Dot product into r (can overwrite because don't need mag anymore, norm vec in q). 
  4) Parallel over num prev vecs: Decumulate in a for inplace orthonormalizatoin. This is a reduction
*/

/*-----------------------------------------------------------------------------------
 * Manycore. Using PolyBench GPU parallelization strategy. No scratchpad use
 *---------------------------------------------------------------------------------*/

// compute magnitude of normalized vector to project on
// polybench does this sequentially because its a reduction
// might consider doing this in parallel if large enough?
void u_magnitude_manycore_baseline(DTYPE *a, DTYPE *r, int numVectors, int vectorLen, int k, int tid, int dim) {
  if (tid == 0) {
    DTYPE sqrMagnitude = 0;
    for (int i = 0; i < vectorLen; i++) {
      sqrMagnitude += a[i * numVectors + k] * a[i * numVectors + k];
    }
    r[k * numVectors + k] = sqrtf(sqrMagnitude);
  }
}

// normalize the orthogonal vector u
// parallelize over the length of the vector
void u_normalize_manycore_baseline(DTYPE *a, DTYPE *r, DTYPE *q, int numVectors, int vectorLen, int k, int tid, int dim) {
  int start = ((tid + 0) * vectorLen) / dim;
  int end   = ((tid + 1) * vectorLen) / dim;

  // make sure r is cached
  DTYPE r_cache = r[k * numVectors + k];

  for (int i = start; i < end; i++) {
    // q[i * numVectors + k] = a[i * numVectors + k] / r_cache;

    // NOTE this adds a fmv instruction. no way around this unless want to modify the compiler to treat instruction as floating point
    STORE_NOACK(a[i * numVectors + k] / r_cache, &q[i * numVectors + k], 0);
  }

  asm volatile("fence\n\t");
}

// do the dotproduct with every subsequent vector and subtract from it
// parallelize across each subsequent vector
void u_dot_subtract_manycore_baseline(DTYPE *a, DTYPE *r, DTYPE *q, int numVectors, int vectorLen, int k, int tid, int dim) {
  // number of vectors we need to project on
  int numProjs = numVectors - ( k + 1 );
  int start = ( k + 1 ) + ( ( ( tid + 0 ) * numProjs ) / dim );
  int end   = ( k + 1 ) + ( ( ( tid + 1 ) * numProjs ) / dim );

  // for each subsequent vector we need to off its component of the k'th vector
  for (int j = start; j < end; j++) {

    // make sure the operation is done locally
    // r[k * numVectors + j] = 0.0f;
    DTYPE r_cache = 0.0f;

    // do the dot product with j'th vector that needs the component of the k'th vector taken off
    for (int i = 0; i < vectorLen; i++) {
      // r[k * numVectors + j] += q[i * numVectors + k] * a[i * numVectors + j];
      r_cache += q[i * numVectors + k] * a[i * numVectors + j];
    }

    // take off the projection of the j'th vector onto orthornal k
    // (we've just finished the computation of the projection after the dot product)
    for (int i = 0; i < vectorLen; i++) {
      // a[i * numVectors + j] -= q[i * numVectors + k] * r[k * numVectors + j];
      // a[i * numVectors + j] -= q[i * numVectors + k] * r_cache;
      DTYPE val = a[i * numVectors + j] - q[i * numVectors + k] * r_cache;
      STORE_NOACK(val, &a[i * numVectors + j], 0);
    }
  }

  asm volatile("fence\n\t");
}

/*-----------------------------------------------------------------------------------
 * Staging
 *---------------------------------------------------------------------------------*/

// for some reason fails if inlined
// dot product. two parts
// 1) do local multiplication and accumulation (in vector/manycore)
// 2) do reduction always using manycore version
void __attribute__((optimize("-fno-inline"))) gram_schmidt(DTYPE *a, DTYPE *r, DTYPE *q, 
    int ptid, int vtid, int dim, int numVectors, int vectorLen, int groupId, int numGroups,
    int mask, int used
  ) {

    #ifndef USE_VEC
    // loop over each non-othorgonal vector a successively orthonormalize
    // dont need to do orthonormalization part on the last step b/c no future vectors to remove component from
    // TODO technically don't have to do last iteration b/c no real works done but polybench does this so keep
    for (int k = 0; k < numVectors; k++) {
      // compute magnitude of the vector
      u_magnitude_manycore_baseline(a, r, numVectors, vectorLen, k, ptid, dim);

      pthread_barrier_wait(&start_barrier);

      // normalize the vector
      u_normalize_manycore_baseline(a, r, q, numVectors, vectorLen, k, ptid, dim);

      pthread_barrier_wait(&start_barrier);

      // apply projection of this vector onto each vector that hasn't been orthonormalized
      u_dot_subtract_manycore_baseline(a, r, q, numVectors, vectorLen, k, ptid, dim);

      pthread_barrier_wait(&start_barrier);
    }
    #else
    // loop over each non-othorgonal vector a successively orthonormalize
    // dont need to do orthonormalization part on the last step b/c no future vectors to remove component from
    // TODO technically don't have to do last iteration b/c no real works done but polybench does this so keep
    for (int k = 0; k < numVectors; k++) {
      // compute magnitude of the vector
      u_magnitude_manycore_baseline(a, r, numVectors, vectorLen, k, ptid, dim);

      pthread_barrier_wait(&start_barrier);

      // normalize the vector
      if (used)
        tril_u_normalize(mask, a, r, q, numVectors, vectorLen, k, ptid, groupId, numGroups, vtid);
      // u_normalize_manycore_baseline(a, r, q, numVectors, vectorLen, k, ptid, dim);


      pthread_barrier_wait(&start_barrier);

      // apply projection of this vector onto each vector that hasn't been orthonormalized
      if (used)
        tril_u_dot_subtract(mask, a, r, q, numVectors, vectorLen, k, ptid, groupId, numGroups, vtid);
      // u_dot_subtract_manycore_baseline(a, r, q, numVectors, vectorLen, k, ptid, dim);

      pthread_barrier_wait(&start_barrier);
    }

    #endif

}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *r, DTYPE *q, int numVectors, int vectorLen,
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

  // get behavior of each core
  #ifdef USE_VEC
  int mask = getSIMDMask(&cinfo);
  #else
  int mask = 0;
  #endif

  // single barrier before kernel start
  pthread_barrier_wait(&start_barrier);

  MOVE_STACK_ONTO_SCRATCHPAD();

  // gramschmidt
  gram_schmidt(a, r, q, ptid, vtid, pdim, numVectors, vectorLen, unique_id, total_groups, mask, used);

  // restore stack pointer
  RECOVER_DRAM_STACK();

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *r, DTYPE *q, int numVectors, int vectorLen,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->r = r;
  args->q = q;
  args->numVectors = numVectors;
  args->vectorLen  = vectorLen;
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
  
  kernel(a->a, a->r, a->q, a->numVectors, a->vectorLen,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
