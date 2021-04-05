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
#include "util.h"

#if defined(PER_CORE_SIMD)
#include <riscv_vector.h>
#endif

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
      // aT[i*a_row+j] = a[j*a_col+i];
      FSTORE_NOACK(a[j*a_col+i], &aT[i*a_row+j], 0);
    }
  }
  FENCE();
}


// compute each mean across each vector (single dimension)
void mean_manycore(DTYPE *mean, DTYPE *data, int N, int M, int tid, int dim) {
  int start = ((tid + 0) * N) / dim;
  int end   = ((tid + 1) * N) / dim;

  #ifdef MANYCORE_PREFETCH
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);
  #endif

  for (int i = start; i < end; i++) { // TODO remove +1, keep for now for eq
    DTYPE mean_i = 0.0f;

    #ifdef PER_CORE_SIMD
    int chunk = M;
    for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
      l = vsetvl_e32m1(chunk);

      int base_j = M - chunk;

      vfloat32m1_t vdata = vle32_v_f32m1(&data[i * M + base_j]);

      // sum
      vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
      vfloat32m1_t vsum = vfredsum_vs_f32m1_f32m1(vdata, vdata, vzero);

      // update the accumulation
      mean_i += vfmv_f_s_f32m1_f32(vsum);
    }
    #elif defined(MANYCORE_PREFETCH)
    for (int j = 0; j < M; j+=MEAN_PREFETCH_LEN) {
      prefetch_mean_frame(data, i, j, &sp, M);

      FRAME_START(MEAN_PREFETCH_LEN);
      #pragma GCC unroll(16)
      for (int u = 0; u < MEAN_PREFETCH_LEN; u++) {
        mean_i += sp_ptr[sp + u];
      }
      END_FRAME();

      sp += MEAN_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
    }
    #else

    // compute mean
    #pragma GCC unroll(16)
    for (int j = 0; j < M; j++) {
      mean_i += data[i * N + j];
    }
    #endif
    mean_i /= (DTYPE)FLOAT_N;

    // TODO dont need this
    FSTORE_NOACK(mean_i, &mean[i], 0);

    #ifdef PER_CORE_SIMD
    chunk = M;
    for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
      l = vsetvl_e32m1(chunk);

      int base_j = M - chunk;

      vfloat32m1_t vdata = vle32_v_f32m1(&data[i * M + base_j]);

      vdata = vfsub_vf_f32m1(vdata, mean_i);

      vse32_v_f32m1(&data[i * M + base_j], vdata);
    }
    #elif defined(MANYCORE_PREFETCH)
    for (int j = 0; j < M; j+=MEAN_PREFETCH_LEN) {
      prefetch_mean_frame(data, i, j, &sp, M);

      FRAME_START(MEAN_FRAME_SIZE);
      #pragma GCC unroll(16)
      for (int u = 0; u < MEAN_PREFETCH_LEN; u++) {
        DTYPE dat = sp_ptr[sp + u] - mean_i;
        FSTORE_NOACK(dat, &data[i * N + j + u], 0);
      }
      END_FRAME();

      sp += MEAN_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
    }
    #else
    #pragma GCC unroll(16)
    for (int j = 0; j < M; j++) {
      DTYPE dat = data[i * M + j] - mean_i;
      FSTORE_NOACK(dat, &data[i * M + j], 0);
    }
    #endif
  }
  FENCE();
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

      #ifdef PER_CORE_SIMD
      int chunk = M;
      for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
        l = vsetvl_e32m1(chunk);

        int base_j = M - chunk;

        // vec loads
        vfloat32m1_t vi1 = vle32_v_f32m1(&data[i1 * N + base_j]);
        vfloat32m1_t vi2 = vle32_v_f32m1(&data[i2 * N + base_j]);

        // multiple together
        vfloat32m1_t vs = vfmul_vv_f32m1(vi1, vi2);

        // sum
        vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
        vs = vfredsum_vs_f32m1_f32m1(vs, vs, vzero);

        // update the accumulation
        symmat_idx += vfmv_f_s_f32m1_f32(vs);;
      }
      #elif defined(MANYCORE_PREFETCH)
      for (int j = 0; j < M; j+=COVAR_PREFETCH_LEN) {
        prefetch_covar_frame(data, i1, i2, j, &sp, M);

        FRAME_START(COVAR_FRAME_SIZE);
        #pragma GCC unroll(16)
        for (int u = 0; u < COVAR_PREFETCH_LEN; u++) {
          symmat_idx += sp_ptr[sp + u] * sp_ptr[sp + COVAR_PREFETCH_LEN + u];
        }
        END_FRAME();
        sp += COVAR_FRAME_SIZE;
        sp = sp % POST_FRAME_WORD;
      }
      #else
      #pragma GCC unroll(16)
      for (int j = 0; j < M; j++) {
        symmat_idx += data[i1 * N + j] * data[i2 * N + j];
      }
      #endif

      FSTORE_NOACK(symmat_idx, &symmat[i2 * N + i1], 0);
      FSTORE_NOACK(symmat_idx, &symmat[i1 * N + i2], 0);
      // symmat[j2 * (M+1) + j1] = symmat_idx;
      // symmat[j1 * (M+1) + j2] = symmat_idx;
    }
  }
  FENCE();
}

#ifdef LONGLINES

// partial sum reduction offload
void mean_reduction(DTYPE *out, int baseGroupId, int numGroups, int N, int M, 
    int ptid, int *fwders) {
  // chunk over vector groups. note all might not do the same amount of work
  int max_chunk_size = ceilToInt((float)N / (float)numGroups);

  // cache sp ptrs to avoid global load
  SETUP_REDUCTION_CORE(fwders, ptid, N, baseGroupId, numGroups);

  for (int cnt = 0; cnt < max_chunk_size; cnt+=ACCUM_GRANULARITY) {

    SETUP_GROUP_ITERATION_CHUNKED_1NEST(baseGroupId, numGroups, cnt, max_chunk_size);

    // SETUP_GROUP_ITERATION_CHUNKED(baseGroupId, numGroups, cnt);

    REDUCE_SYNC_WITH_SCALAR(group_start, spPtrs, flat_iter);



    // printf("%d %d %d\n", cnt, group_start[0], expected_elements);
    // wait for frame and then do sum
    FRAME_START(expected_elements);

    for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {

      #pragma GCC unroll(4)
      for (int a = 0; a < ACCUM_GRANULARITY; a++) {
        DTYPE sum = 0.0f;

        int sum_offset = g * PER_CORE_MAILER_FRAME + a * SUB_FRAME_SIZE;

        #pragma GCC unroll(16)
        for (int k = 0; k < PER_CORE_MAILER_FRAME; k++) {
          sum += sp_ptr[sum_offset + sp_self + k];
        }

        sum /= (DTYPE)FLOAT_N;

        int i = group_start[g];
        if (i < 0 || i + a >= group_end[g]) continue; // TODO test b/c wrong before??

        FSTORE_NOACK(sum, &out[i + a], 0);
      }

    }
    sp_self += MAILER_FRAME_SIZE;
    sp_self = sp_self % MAILER_POST_FRAME_WORD; // TOOD branch better??
    REMEM(expected_elements);
  }
}

// partial sum reduction offload
void covar_reduction(DTYPE *symmat, int baseGroupId, int numGroups, int N, int M, 
    int ptid, int *fwders) {
  // chunk over vector groups. note all might not do the same amount of work
  int max_chunk_size = ceilToInt((float)N / (float)numGroups);

  // cache sp ptrs to avoid global load
  SETUP_REDUCTION_CORE(fwders, ptid, N, baseGroupId, numGroups);

  for (int cnt = 0; cnt < max_chunk_size; cnt++) {

    SETUP_GROUP_ITERATION_STRIDED(baseGroupId, numGroups, cnt, N);

    for (int i2 = group_start[0]; i2 < N; i2+=ACCUM_GRANULARITY) {      

      REDUCE_SYNC_WITH_SCALAR(group_start, spPtrs, flat_iter);

      // int expected_elements = 0;  
      // for (int a = 0; a < ACCUM_GRANULARITY; a++) {
      //   if (i2 + a < N)
      //     expected_elements += PER_CORE_FULL_MAILER_FRAME*NUM_GROUPS_PER_PIPE;
      // }
      // printf("%d\n", expected_elements);
      // if (expected_elements == 0) return;

      // wait for frame and then do sum
      FRAME_START(expected_elements);

      for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {

        #pragma GCC unroll(4)
        for (int a = 0; a < ACCUM_GRANULARITY; a++) {
          DTYPE sum = 0.0f;

          int sum_offset = g * PER_CORE_MAILER_FRAME + a * SUB_FRAME_SIZE;

          #pragma GCC unroll(16)
          for (int k = 0; k < PER_CORE_MAILER_FRAME; k++) {
            sum += sp_ptr[sum_offset + sp_self + k];
          }

          int i1 = group_start[g];
          int i2_eff = i2 + g + a;
          if (i1 < 0 || i2_eff >= N ) continue;

          FSTORE_NOACK(sum, &symmat[i2_eff * M + i1], 0);
          FSTORE_NOACK(sum, &symmat[i1 * M + i2_eff], 0);
        }

      }
      sp_self += MAILER_FRAME_SIZE;
      sp_self = sp_self % MAILER_POST_FRAME_WORD; // TOOD branch better??
      REMEM(expected_elements);
    }
  }
}
#endif

// void mean_sum(DTYPE* data, DTYPE *mean, int N, int M) {
//   for (int i = 0; i < N; i++) {
//     DTYPE temp = 0.0f;
//     for (int j = 0; j < M; j++) {
//       temp+=data[i * M + j];
//     }
//     mean[i] = temp/(DTYPE)FLOAT_N;
//   }
// }


// void reduce_the_deuce(DTYPE *data, DTYPE *mean, int N, int M) {
//   for (int i = 0; i < N; i++) {
//     for (int j = 0; j < M; j++) {
//       DTYPE val = data[i * M + j] - mean[j];
//       FSTORE_NOACK(val, &data[i*M+j], 0);
//     }
//   }
// }

void __attribute__((optimize("-fno-inline"))) covar(
    DTYPE *data, DTYPE *dataT, DTYPE *mean, DTYPE *symmat,
    int ptid, int vtid, int dim, int N, int M, int groupId, int numGroups,
    int mask, int used, int ptidMailer, int isMailer, int *ptidFwder, int linkId
  ) {


    transpose_manycore(data, M, N, dataT, ptid, dim);

    // longlines non-transposed size
    #ifdef LONGLINES
    if (isMailer) {
      SET_PREFETCH_MASK(MAILER_NUM_FRAMES, MAILER_FRAME_SIZE, &start_barrier); 
    } else {
      SET_PREFETCH_MASK(NUM_MEAN_FRAMES, MEAN_FRAME_SIZE, &start_barrier);
    }
    if (used)
      tril_mean(mask, mean, dataT, N, M, ptid, groupId, 
        numGroups, vtid, ptidMailer, linkId);
    else if (isMailer)
      mean_reduction(mean, groupId, numGroups, N, M, ptid, ptidFwder);
    // if (ptid == 0)
    //   mean_sum(dataT, mean, N, M);

    // need to add reduce kernel as well
    SET_PREFETCH_MASK(NUM_REDUCE_FRAMES, REDUCE_FRAME_SIZE, &start_barrier);
    // if (ptid == 0)
      // reduce_the_deuce(dataT, mean, N, M);

    if (used)
      tril_reduce(mask, mean, dataT, N, M, ptid, groupId, 
        numGroups, vtid, ptidMailer, linkId);

    if (isMailer) {
      SET_PREFETCH_MASK(MAILER_NUM_FRAMES, MAILER_FRAME_SIZE, &start_barrier); 
    } else {
      SET_PREFETCH_MASK(NUM_COVAR_FRAMES, COVAR_FRAME_SIZE, &start_barrier);
    }
    if (used)
      tril_covar(mask, symmat, dataT, N, M, ptid, groupId, 
        numGroups, vtid, ptidMailer, linkId);
    else if (isMailer)
      covar_reduction(symmat, groupId, numGroups, N, M, ptid, ptidFwder);
    // covar_manycore(symmat, dataT, N, M, ptid, dim);

    #else

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
      tril_mean(mask, mean, dataT, N, M, ptid, groupId, 
        numGroups, vtid, ptidMailer, linkId);

    SET_PREFETCH_MASK(NUM_COVAR_FRAMES, COVAR_FRAME_SIZE, &start_barrier);
    if (used)
      tril_covar(mask, symmat, dataT, N, M, ptid, groupId, 
        numGroups, vtid, ptidMailer, linkId);
    #endif
    #endif

}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *data, DTYPE *dataT, DTYPE *mean, DTYPE *symmat, int N, int M,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y) {
  
  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0) {
    stats_on();
  }

  #if VECTOR_LEN==4
  SET_USEFUL_VARIABLES_V4(ptid_x, ptid_y, pdim_x, pdim_y);
  #elif VECTOR_LEN==16
  SET_USEFUL_VARIABLES_V16(ptid_x, ptid_y, pdim_x, pdim_y);
  #else
  SET_USEFUL_VARIABLES_MANYCORE(ptid_x, ptid_y, pdim_x, pdim_y);
  #endif

  #ifdef LONGLINES
  SETUP_REDUCE_CONFIG();
  #else
  SETUP_REDUCE_CONFIG_NULL();
  #endif

  // need to set vlen here so doesn't cause squash in vector core on change in value
  #ifdef PER_CORE_SIMD
  vsetvl_e32m1(PER_CORE_SIMD_LEN);
  #endif

  MOVE_STACK_ONTO_SCRATCHPAD();

  // compute covariance
  covar(data, dataT, mean, symmat, ptid, vtid, pdim, N, M, unique_id, total_groups,
    mask, used, ptidMailer, isMailer, ptidFwders, linkId);

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

  // reset scratchpad config
  SET_PREFETCH_MASK(0, 0, &start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
