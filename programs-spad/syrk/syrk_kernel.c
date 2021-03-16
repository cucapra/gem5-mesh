#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "syrk.h"
#include "spad.h"
#include "bind_defs.h"
#include "util.h"

#ifdef NESTED_SIMD
#include <riscv_vector.h>
#endif

#define ROFL_COP

// #define SCALAR_CORE
// #define VECTOR_CORE

/*-----------------------------------------------------------------------------------
 * Vector versions of the kernels.
 *---------------------------------------------------------------------------------*/

#ifdef USE_VEC

#ifdef SCALAR_IS_MAILER
// fetch c value at beginning of the frame
inline void do_sum_prefetch(DTYPE *c, int i, int j_base, int N, int sp_idx) {
  #pragma GCC unroll(8)
  for (int j = 0; j < ACCUM_GRANULARITY; j++) {
    int j_idx = j + j_base;
    VPREFETCH_L(sp_idx + j * MAILER_FRAME_SIZE, &c[i * N + j_idx], 0, 1, TO_SELF);
  }
}

// do reduction on scalar core
// each core in vector group should have sent a message in a frame
inline void do_sum(DTYPE *c, int i, int j_base, int N, DTYPE *sp_ptr, int *sp_idx) {
  // merge frame into full sum
  #pragma GCC unroll(8)
  for (int j = 0; j < ACCUM_GRANULARITY; j++) {
    FRAME_START(MAILER_FRAME_SIZE);
    DTYPE sum = sp_ptr[*sp_idx + 0] * beta;
    #pragma unroll(16)
    for (int i = MAILER_OFFSET; i < MAILER_FRAME_SIZE; i++) {
      sum += sp_ptr[*sp_idx + i];
    }
    int j_idx = j + j_base;
    STORE_NOACK(sum, &c[i * N + j_idx], 0);
    REMEM(MAILER_FRAME_SIZE);
    (*sp_idx)+=MAILER_FRAME_SIZE;
    *sp_idx = *sp_idx % MAILER_POST_FRAME_WORD;
  }
}
#endif

// fetch first value to first core
inline void do_sum_prefetch(int sp, int starting_vtid, DTYPE *c, int i, int j, int N, int M) {
    int sp_start_sum = (sp + INNER_FRAME_SIZE * (M / K_STRIDE)) % POST_FRAME_WORD;
    VPREFETCH_L(sp_start_sum, &c[i * N + j], starting_vtid, 1, TO_ONE_CORE);
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

void tril_syrk(int mask, DTYPE *a, DTYPE *c, int N, int M, 
                  int ptid, int groupId, int numGroups, int vtid,
                  int ptidMailer, int linkId, int numGroupsPerMailer) {


  #ifdef SCALAR_CORE

  VECTOR_EPOCH(mask);

  // chunk over vector gorups
  int start = ((groupId + 0) * N) / numGroups;
  int end   = ((groupId + 1) * N) / numGroups;

  // printf("ptid %d %d->%d (%d)\n", ptid, start, end, end-start);

  #ifndef LONGLINES
  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);
  #endif

  int startOffset = min(INIT_OFFSET, M);

  int numCompleted = 0;

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * N) / numGroups;
  #ifndef LONGLINES
  start = roundUp(start, VECTOR_LEN);
  #endif
  int i = start;
  int j = vtid;
  int sp = 0;
  DTYPE c_ij;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);

  #ifdef SCALAR_IS_MAILER
  int sp_origin = vtid + MAILER_OFFSET;
  #else
  int sp_origin = (linkId * PER_CORE_MAILER_FRAME) + vtid;
  #endif
  DTYPE* sp_origin_ptr = (DTYPE*)getSpAddr(ptidMailer, 0);

  // rename some variables
  #ifdef ROFL_COP
  int starting_vtid = linkId;
  int ending_vtid = numGroupsPerMailer;
  // one core will do all the work
  j = 0;
  #endif

  #ifdef NESTED_SIMD
  vsetvl_e32m1(NESTED_SIMD_VLEN);
  vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
  #endif
  #endif

  #ifdef SCALAR_CORE
  int sp = 0;
  int sp_self = 0;
  #ifdef SCALAR_IS_MAILER
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #else
  volatile int *sp_ptr = (int*)getSpAddr(ptid, 0);
  #endif

  // DTYPE* sp_origin_ptr = (DTYPE*)getSpAddr(ptidMailer, 0);

  // int init_offset = min(INIT_OFFSET, M);

  for (int i = start; i < end; i++) {

    #ifdef LONGLINES

    #if INIT_FRAMES==0
    for (int j = 0; j < M; j+=J_STRIDE) {
      ISSUE_VINST(vec_body_init_label);

      #ifdef SCALAR_IS_MAILER
      do_sum_prefetch(c, i, j, N, sp_self);
      #elif defined(ROFL_COP)
      do_sum_prefetch(sp, linkId, c, i, j, N, M);
      #endif

      for (int k = 0; k < M; k+=K_STRIDE) {
        prefetch_inner_frame(a, i, j, k, &sp, M);
        ISSUE_VINST(vec_body_label);
      }

      #if !defined(SCALAR_IS_MAILER) && !defined(ROFL_COP)
      // wait for mailer to be ready
      while (1) {
        int wait_val = sp_ptr[POST_FRAME_WORD]; 
        if (numCompleted + 1 < wait_val + FRAMES_TO_SYNC_AFTER) break;
      }
      numCompleted++;
      #endif

      #ifdef ROFL_COP
      ISSUE_VINST(vec_body_pre_end_label);
      sp = (sp + INNER_FRAME_SIZE) % POST_FRAME_WORD;
      #endif

      ISSUE_VINST(vec_body_end_label);

      

      #ifdef SCALAR_IS_MAILER
      do_sum(c, i, j, N, sp_ptr, &sp_self);
      #endif
    }

    #else
    int j = 0;

    // get ahead
    ISSUE_VINST(vec_body_init_label);

    #ifdef ROFL_COP
    do_sum_prefetch(sp, linkId, c, i, j, N, M);
    #endif

    for (int k = 0; k < startOffset; k+=K_STRIDE) {
      prefetch_inner_frame(a, i, j, k, &sp, M);
    }

    // do first inner loop
    for (int k = startOffset; k < M; k+=K_STRIDE) {
      prefetch_inner_frame(a, i, j, k, &sp, M);
      ISSUE_VINST(vec_body_label);
    }

    #ifdef ROFL_COP
    // ISSUE_VINST(vec_body_pre_end_label);
    sp = (sp + INNER_FRAME_SIZE) % POST_FRAME_WORD;
    #endif

    // steady state
    for (j = 1; j < M; j+=J_STRIDE) {

      #ifdef SCALAR_IS_MAILER
      // prefetch what is needed for sum
      do_sum_prefetch(c, i, j - ACCUM_GRANULARITY, N, sp_self);
      #endif

      #ifdef ROFL_COP
      do_sum_prefetch(sp, linkId, c, i, j, N, M);
      #endif


      for (int k = 0; k < M; k+=K_STRIDE) {

        prefetch_inner_frame(a, i, j, k, &sp, M);
        ISSUE_VINST(vec_body_label);

        // finish loop at a weird time
        if (k + K_STRIDE == startOffset) {

          #if !defined(SCALAR_IS_MAILER) && !defined(ROFL_COP)
          // wait for mailer to be ready
          while (1) {
            int wait_val = sp_ptr[POST_FRAME_WORD];

            // plus 1 b/c will send another if allowed to pass
            if (numCompleted + 1 < wait_val + FRAMES_TO_SYNC_AFTER) break;
          }

          // printf("reset value %d %d\n", ptid, j);
          numCompleted++;
          #endif

          #ifdef ROFL_COP
          ISSUE_VINST(vec_body_pre_end_label);
          #endif

          // printf("write frame %d %d\n", ptid, j-1);
          ISSUE_VINST(vec_body_end_label);
          ISSUE_VINST(vec_body_init_label);

        }
      }

      #ifdef ROFL_COP
      sp = (sp + INNER_FRAME_SIZE) % POST_FRAME_WORD;
      #endif

      #ifdef SCALAR_IS_MAILER
      // every so often need to complete the sum
      // maybe want to put this somewhere else, so in between vinst and such?
      if (j % ACCUM_GRANULARITY == 0)
        do_sum(c, i, j - ACCUM_GRANULARITY, N, sp_ptr, &sp_self);
      #endif
    }

    // draining. do the last vissue corresponding to the initial round of prefetch
    for (int k = M - startOffset; k < M; k+=K_STRIDE) {
      ISSUE_VINST(vec_body_label);
    }


    #if !defined(SCALAR_IS_MAILER) && !defined(ROFL_COP)
    while (1) {
      int wait_val = sp_ptr[POST_FRAME_WORD];
      if (numCompleted + 1 < wait_val + FRAMES_TO_SYNC_AFTER) break;
    }
    numCompleted++;
    #endif

    #ifdef ROFL_COP
    ISSUE_VINST(vec_body_pre_end_label);
    #endif

    ISSUE_VINST(vec_body_end_label);
    #ifdef SCALAR_IS_MAILER
    do_sum_prefetch(c, i, M - ACCUM_GRANULARITY, N, sp_self);
    do_sum(c, i, M - ACCUM_GRANULARITY, N, sp_ptr, &sp_self);
    #endif
    #endif

    #else
    for (int j = 0; j < M; j+=J_STRIDE) {
      prefetch_outer_frame(c, i, j, &sp, N);
      ISSUE_VINST(vec_body_init_label);

      // warmup 
      for (int k = 0; k < startOffset; k+=K_STRIDE) {
        prefetch_inner_frame(a, i, j, k, &sp, M);
      }

      // steady-state
      for (int k = startOffset; k < M; k+=K_STRIDE) {
        prefetch_inner_frame(a, i, j, k, &sp, M);
        ISSUE_VINST(vec_body_label);
      }

      // cooldown
      for (int k = M - startOffset; k < M; k+=K_STRIDE) {
        ISSUE_VINST(vec_body_label);
      }

      ISSUE_VINST(vec_body_end_label);
    }
    #endif
  }

  // // get ahead
  // prefetch_outer_frame(c, start, 0, &sp, N);
  // for (int k = 0; k < INIT_OFFSET; k+=INNER_PREFETCH_LEN) {
  //   prefetch_inner_frame(a, start, 0, k, &sp, M);
  // }

  // // do first inner loop
  // for (int k = INIT_OFFSET; k < M; k+=INNER_PREFETCH_LEN) {
  //   prefetch_inner_frame(a, start, 0, k, &sp, M);
  //   ISSUE_VINST(vec_body_label);
  // }

  // // steady state
  // for (int i = start; i < end; i++) {
  //   int startJ = 0;
  //   if (i == start) startJ += VECTOR_LEN;
  //   for (int j = startJ; j < M; j+=VECTOR_LEN) {
  //     prefetch_outer_frame(c, i, j, &sp, N);

  //     for (int k = 0; k < M; k+=INNER_PREFETCH_LEN) {
  //       prefetch_inner_frame(a, i, j, k, &sp, M);
  //       ISSUE_VINST(vec_body_label);
  //     }
  //   }
  // }

  // // draining. do the last vissue corresponding to the initial round of prefetch
  // for (int k = N - INIT_OFFSET; k < N; k+=INNER_PREFETCH_LEN) {
  //   ISSUE_VINST(vec_body_label);
  // }
  #endif
  
  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;
  do { 

    asm("trillium vissue_delim until_next vec_body_init");

    #ifndef LONGLINES
    FRAME_START(OUTER_FRAME_SIZE);
    // c[i * N + j] *= beta;
    // printf("c %f ?= %f\n", sp_ptr[sp], c[i*N+j]);
    c_ij = sp_ptr[sp + 0] * beta;
    REMEM(OUTER_FRAME_SIZE);
    // pad so num regions possible regions is lower
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    sp+=OUTER_FRAME_SIZE;
    sp = sp % POST_FRAME_WORD;
    #else
    #ifdef NESTED_SIMD
    // NOTE MUST NEVER CHANGE THIS VALUE B/C CANT SQUASH IN VCORES!!!
    size_t l = vsetvl_e32m1(NESTED_SIMD_VLEN);
    #endif
    c_ij = 0;
    #endif

    do {
      asm("trillium vissue_delim if_begin vec_body");
  
      // do innermost loop body (k)
      #ifdef NESTED_SIMD
      l = vsetvl_e32m1(NESTED_SIMD_VLEN);
      #endif
      
      FRAME_START(INNER_FRAME_SIZE);

      // WARNING if less than INNER_PREFETCH_LEN, creates code gen problem
      // could always just not unroll
      #pragma GCC unroll(32)
      for (int k = 0; k < INNER_PREFETCH_LEN; k+=NESTED_SIMD_VLEN) {
        #ifdef NESTED_SIMD
        // load from scratchpad frame
        vfloat32m1_t vai = vle32_v_f32m1(&sp_ptr[sp + k]);
        vfloat32m1_t vaj = vle32_v_f32m1(&sp_ptr[sp + INNER_PREFETCH_LEN + k]);

        vfloat32m1_t vaa = vfmul_vv_f32m1(vai, vaj);
        vfloat32m1_t vaaa = vfmul_vf_f32m1(vaa, alpha); 

        vfloat32m1_t vcij = vfredsum_vs_f32m1_f32m1(vaaa, vaaa, vzero);
        c_ij += vfmv_f_s_f32m1_f32(vcij);
        #else
        c_ij += alpha * sp_ptr[sp + k] * sp_ptr[sp + INNER_PREFETCH_LEN + k];
        #endif
      }

      // printf("a %f ?= %f %f ?= %f\n", sp_ptr[sp + 0], a[i * M + k], sp_ptr[sp + 1], a[j * M +k]);
      // c[i * N + j] += alpha * a[i * M + k] * a[j * M + k];
      // c_ij += alpha * sp_ptr[sp + 0] * sp_ptr[sp + 1];
      REMEM(INNER_FRAME_SIZE);
      sp+=INNER_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
      asm("trillium vissue_delim end at_jump");
    } while(BH);

    #ifdef ROFL_COP
    asm("trillium vissue_delim until_next vec_body_pre_end");
    // do instructions that dont fit into first block (specifically for vtid == 0)
    // PRED_EQ(vtid, 0); // if pred doesnt work just prefetch value to each core and then write to slot 2?
    
    PRED_EQ(vtid, starting_vtid);
    FRAME_START(ACCUM_GRANULARITY);
    PRED_EQ(0, 0);

    // add current value
    DTYPE start_sum = sp_ptr[sp];
    start_sum *= beta;

    // store into a frame
    // DTYPE *sp_next_ptr = sp_origin_ptr; //rename for clarity
    // int sp_val_idx = sp + 1;

    // only first core does
    PRED_EQ_STORE_NOACK(vtid, starting_vtid, start_sum, &sp_ptr[sp], 0);

    #endif
    asm("trillium vissue_delim if_begin vec_body_end");

    #ifdef LONGLINES
    #ifndef ROFL_COP
    // store partial sum to scalar core
    STORE_NOACK(c_ij, &sp_origin_ptr[sp_origin], 0);
    sp_origin+=MAILER_FRAME_SIZE;
    sp_origin = sp_origin % MAILER_POST_FRAME_WORD;
    #else
    // do reduction systolically on vector cores
    // cant exceed queue length between any two vector cores if want to snake it
    // 10 instructions between frame start and store in this case. should be enough :p

    // ------
    // critical section (must be <10 instructions)
    // seriously had 10 instructions due to predication and needed to remove to get to not deadlock

    // get from frame (make sure scalar core doesn't write to this one)
    FRAME_START(ACCUM_GRANULARITY);

    // add current value
    c_ij += sp_ptr[sp];
    // PRED_EQ(vtid, 0);
    // c_ij *= beta;
    // PRED_EQ(0, 0);

    // store into a frame
    // DTYPE *sp_next_ptr = sp_origin_ptr; //rename for clarity

    // last core doesn't do
    // i think this could mess up with remem on final core which writes to self
    // would need to be store with ack to work (remem done on commit atomically)
    // possible the timing works out though
    // PRED_NEQ(vtid, SUM_END_VTID);
    STORE_NOACK(c_ij, &sp_origin_ptr[sp], 0); 
    // PRED_EQ(0, 0);

    // ------
    // after this point can stall all you want with deadlock

    // free frame
    REMEM(ACCUM_GRANULARITY);

    // if your the last core in snake, do store
    PRED_EQ_STORE_NOACK(vtid, ending_vtid, c_ij, &c[i * N + j], 0);
    // STORE_NOACK(3.0f, &c[i * N + j], 0);

    // do pointer updates
    sp = (sp + INNER_FRAME_SIZE) % POST_FRAME_WORD;

    // handle outer loop
    j+=J_STRIDE;
    if (j >= M) {
      #ifdef ROFL_COP
      j = 0;
      #else
      j = vtid;
      #endif
      i++;
    }
    #endif
    #else

    STORE_NOACK(c_ij, &c[i * N + j], 0);

    // handle outer loops
    j+=J_STRIDE;
    if (j >= M) {
      j = vtid;
      i++;
    }
    #endif
    asm("trillium vissue_delim end at_jump");

  } while(BHO);
  #endif
  
  // Clean up on the vector cores.
#ifdef SCALAR_CORE
  ISSUE_VINST(vector_return_label);
#elif defined VECTOR_CORE
  asm("trillium vissue_delim return vector_return");
  return;
#endif

#ifdef SCALAR_CORE
  // devec with unique tag
  DEVEC(devec_0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  asm("trillium vissue_delim return scalar_return");  // XXX is this real???
  return;
#endif

  // Glue points!
#ifdef SCALAR_CORE
init_label:
  asm("trillium glue_point vector_init");
#if INIT_FRAMES==0
exit(1);
#endif
vec_body_init_label:
  asm("trillium glue_point vec_body_init");
#if INIT_FRAMES==0
exit(1);
#endif
vec_body_label:
  asm("trillium glue_point vec_body");
#if INIT_FRAMES==0
exit(1);
#endif
vec_body_end_label:
  asm("trillium glue_point vec_body_end");
#if INIT_FRAMES==0
exit(1);
#endif
#ifdef ROFL_COP
vec_body_pre_end_label:
  asm("trillium glue_point vec_body_pre_end");
#if INIT_FRAMES==0
exit(1);
#endif
#endif
vector_return_label:
  asm("trillium glue_point vector_return");
#if INIT_FRAMES==0
exit(1);
#endif
#endif

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