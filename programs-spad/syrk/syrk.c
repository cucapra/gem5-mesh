#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "syrk.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "syrk_kernel.h"

#if defined(PACKED_SIMD) || defined(NESTED_SIMD) 
#include <riscv_vector.h>
#endif

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

  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);

  for (int i = start; i < end; i++) {
    for (int j = 0; j < M; j++) {
      // TODO not prefetching outframe but should be negligable
      DTYPE c_ij = c[i * N + j] * beta;
      
      #ifdef PACKED_SIMD
      int chunk = M;
      for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
        l = vsetvl_e32m1(chunk);

        int base_k = M - chunk;

        vfloat32m1_t vai = vle32_v_f32m1(&a[i * M + base_k]);
        vfloat32m1_t vaj = vle32_v_f32m1(&a[j * M + base_k]);

        // alpha * a[i*M+k] * a[j*M+k]
        vfloat32m1_t vaa = vfmul_vv_f32m1(vai, vaj);
        vfloat32m1_t vaaa = vfmul_vf_f32m1(vaa, alpha);

        // sum
        vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
        vfloat32m1_t vcij = vfredsum_vs_f32m1_f32m1(vaaa, vaaa, vzero);

        // update the accumulation
        c_ij += vfmv_f_s_f32m1_f32(vcij);
      }

      // TODO prob size needs to be greater than 64
      #elif defined(MANYCORE_PREFETCH)
      for (int k = 0; k < M; k+=INNER_PREFETCH_LEN) {
        prefetch_inner_frame(a, i, j, k, &sp, M);

        FRAME_START();
        #pragma GCC unroll(16)
        for (int kin = 0; kin < INNER_PREFETCH_LEN; kin++) {
          c_ij += alpha * sp_ptr[sp + kin] * sp_ptr[sp + INNER_PREFETCH_LEN + kin];
        }
        END_FRAME();

        sp += INNER_FRAME_SIZE;
        sp = sp % POST_FRAME_WORD;
      }
      #else
      #pragma GCC unroll(16)
      for (int k = 0; k < M; k++) {
        c_ij += alpha * a[i * M + k] * a[j * M + k];
      }
      #endif
      STORE_NOACK(c_ij, &c[i * N + j], 0);
    }
  }

  asm volatile("fence\n\t");
}

/*-----------------------------------------------------------------------------------
 * Staging
 *---------------------------------------------------------------------------------*/

// #ifdef LONGLINES

inline int get_group_start(int id, int N, int numGroups) {
  return (( id + 0 ) * N) / numGroups;
}

inline int get_group_end(int id, int N, int numGroups) {
  return (( id + 1 ) * N) / numGroups;
}

inline int get_group_len(int id, int N, int numGroups) {
  return get_group_end(id, N, numGroups) - get_group_start(id, N, numGroups);
}

inline int ceilToInt(float val) {
  int base = (int)val;
  return base + 1;// ceilf() not working so do cheap version
}

// TODO making assumption that if multiple groups then are consecutive
void mailer(DTYPE *c, int baseGroupId, int numGroups, int N, int M, 
    int ptid, int *fwders, int numGroupsToSum) {
  // chunk over vector groups. note all might not do the same amount of work
  int max_chunk_size = ceilToInt((float)N / (float)numGroups);

  // printf("%d fwders %d %d %d\n", ptid, fwders[0], fwders[1], fwders[2]);

  int sp_self = 0;
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  // printf("%d %d %d %d %d %f\n", 
    // ptid, max_chunk_size, numGroupsToSum, baseGroupId, fwders[0], ceilf((float)N/(float)numGroups));
  for (int cnt = 0; cnt < max_chunk_size; cnt++) {
    // printf("%d %d\n", ptid, cnt);
    int group_start[MAX_GROUP_AFFINITY]; 
    // figure out how many valid elements we're expecting in the frame
    int expected_elements = 0;
    for (int g = 0; g < numGroupsToSum; g++) {
      int gid = baseGroupId + g;
      if (cnt < get_group_len(gid, N, numGroups)) {
        expected_elements+=PER_CORE_SCALAR_FRAME;
        group_start[g] = get_group_start(gid, N, numGroups) + cnt;
      }
      else {
        // printf("%d %d %d %d %d %d cant go\n", ptid, g, gid, N, numGroups, get_group_len(gid, N, numGroups));
        group_start[g] = -1;
      }
    }

    if (expected_elements == 0) return; // TODO not sure the issue
    for (int j = 0; j < M; j+=J_STRIDE) {

      if (j % FRAMES_TO_SYNC_AFTER == 0) {
        // try to do syncronization with cores
        for (int g = 0; g < numGroupsToSum; g++) {
          if (group_start[g] < 0) {
            // printf("%d %d %d skip sync %d\n", ptid, cnt, j, fwders[g]);
            continue;
          }

          // printf("%d %d\n", ptid, j);
          // inform scalar core of the group that ready to go
          volatile int *sp_scalar_ptr = (int*)getSpAddr(fwders[g], 0);
          // printf("start set value %d %d %d %d\n", ptid, fwders[g], g, j);
          while (1) {
            int wait_val = sp_scalar_ptr[POST_FRAME_WORD];
            if (wait_val == 0) break;
          }
          // printf("set value %d %d\n", ptidScalar, j); // gets here
          STORE_NOACK(1, &sp_scalar_ptr[POST_FRAME_WORD], 0); 
          
        }
      }

      // load initial value
      DTYPE sum[MAX_GROUP_AFFINITY];
      for (int g = 0; g < numGroupsToSum; g++) {
        // int gid = baseGroupId + g; // todo do lookup here if not consecutive
      
        // // check if expecting frame
        // // because of the way div works, should have cores with larger ids write
        // // to earlier frames to avoid skips
        // if (cnt >= get_group_len(gid, N, numGroups)) continue;
        // int i = get_group_start(gid, N, numGroups) + cnt;
        int i = group_start[g];
        if (i < 0) continue;
        sum[g] = c[i * N + j] * beta;
      }

      // printf("%d start consume frame %d %d %d %d\n", ptid, cnt, j, expected_elements, numGroupsToSum);

      // wait for frame and then do sum
      FRAME_START(expected_elements);

      // printf("%d consume %d\n", ptid, j);
      for (int g = 0; g < numGroupsToSum; g++) {
        // int gid = baseGroupId + g;

        for (int k = 0; k < PER_CORE_SCALAR_FRAME; k++) {
          sum[g] += sp_ptr[sp_self + k];
        }

        sp_self += PER_CORE_SCALAR_FRAME;

        int i = group_start[g];
        if (i < 0) continue;

        // if (i == 5 && j == 1) printf("%d %d %f %d %d\n", ptid, g, sum[g], cnt, baseGroupId);

        STORE_NOACK(sum[g], &c[i * N + j], 0);

      }
      REMEM(expected_elements);
      sp_self = sp_self % SCALAR_POST_FRAME_WORD;


    }


    // // do reverse order b/c later groups will always have the same or more frames
    // for (int g = 0; g >= -1*numGroupsToSum + 1; g--) {
    //   int gid = baseGroupId + g; // todo do lookup here if not consecutive
      
    //   // check if expecting frame
    //   // because of the way div works, should have cores with larger ids write
    //   // to earlier frames to avoid skips
    //   if (cnt >= get_group_len(gid, N, numGroups)) continue;
      
    //   volatile int *sp_scalar_ptr = (int*)getSpAddr(fwders[g], 0);
    //   int i = get_group_start(gid, N, numGroups) + cnt;

    //   for (int j = 0; j < M; j+=J_STRIDE*ACCUM_GRANULARITY) { // TODO wrong order
    //     // printf("%d %d\n", ptid, j);
    //     // inform scalar core of the group that ready to go
    //     if (j % FRAMES_TO_SYNC_AFTER == 0) {
    //       // printf("start set value %d %d\n", ptidScalar, j);
    //       while (1) {
    //         int wait_val = sp_scalar_ptr[POST_FRAME_WORD];
    //         if (wait_val == 0) break;
    //       }
    //       // printf("set value %d %d\n", ptidScalar, j); // gets here
    //       // sp_scalar_ptr[POST_FRAME_WORD] = 1;
    //       // DOESNT WORK?? SYNC PROB?
    //       STORE_NOACK(1, &sp_scalar_ptr[POST_FRAME_WORD], 0); 
    //     }

    //     // do_sum(c, i, j, N, sp_ptr, &sp_self);

    //     // load values
    //     DTYPE sum[MAX_GROUP_AFFINITY];

        
    //   }
    // }
  }
}
// #endif


void __attribute__((optimize("-fno-inline"))) syrk(
    DTYPE *a, DTYPE *c,
    int ptid, int vtid, int dim, int N, int M, int groupId, int numGroups,
    int mask, int used, int ptidMailer, int isMailer, int *ptidFwder, int numGroupsToSum, int linkId
  ) {

    #ifndef USE_VEC
    syrk_manycore_baseline(a, c, N, M, ptid, dim);
    #else
    if (used)
      tril_syrk(mask, a, c, N, M, ptid, groupId, numGroups, 
        vtid, ptidMailer, linkId, numGroupsToSum);
    #ifdef USE_VEC
    else if (isMailer)
      mailer(c, groupId, numGroups, N, M, ptid, ptidFwder, numGroupsToSum);
    #endif
    #endif

    // printf("finish %d\n", ptid);

}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *c, int N, int M,
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
  #ifdef USE_VEC
  #if VECTOR_LEN==4
  template_info_t tinfo = init_template_4x4_2x2();
  // template_info_t tinfo = init_template_debug();
  #elif VECTOR_LEN==16
  template_info_t tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  vtid = cinfo.vtid_flat;
  vtid_x = cinfo.vtid.x;
  vtid_y = cinfo.vtid.y;
  vdim_x = cinfo.vdim.x;
  vdim_y = cinfo.vdim.y;
  orig_x = cinfo.orig.x;
  orig_y = cinfo.orig.y;
  is_da  = cinfo.is_scalar;
  master_x = cinfo.master.x;
  master_y = cinfo.master.y;
  unique_id = cinfo.unique_id;
  total_groups = cinfo.total_groups;
  used = cinfo.used;

  // if (!used) printf("unused %d %d = %d\n", ptid_x, ptid_y, ptid);

  // printf("ptid %d(%d,%d) da %d vtid %d(%d,%d) dim %d(%d,%d) used? %d\n", 
    // ptid, ptid_x, ptid_y, is_da, vtid, vtid_x, vtid_y, 4, vdim_x, vdim_y, used);

  #else

  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  used   = 1;

  #endif

  // linearize some fields
  vdim = vdim_x * vdim_y;

  // the core that is responsible for doing stores to DRAM if reduction over
  // entire vector group
  int ptidMailer = 0;
  int ptidFwders[MAX_GROUP_AFFINITY];
  int numGroupsPerMailer = 0;
  int linkId = cinfo.link_id[0];
  int isMailer = 0;

  // get behavior of each core
  #ifdef NUM_FRAMES
  // setup up self prefetch
  #ifdef MANYCORE_PREFETCH
  core_config_info_t cinfo = manycore_template(ptid_x, ptid_y, pdim_x, pdim_y);
  int mask = getDebugMask(&cinfo);
  VECTOR_EPOCH(mask);
  #else
  int mask = getSIMDMask(&cinfo);
  #endif

  int ptidOrig_x, ptidOrig_y;
  group_id_to_scalar(&tinfo, unique_id, &ptidOrig_x, &ptidOrig_y);

  #ifdef LONGLINES
  #ifdef SCALAR_IS_MAILER
  ptidMailer = ptidScalar;
  isMailer = is_da;
  #else

  numGroupsPerMailer = tinfo.num_groups_in_template / tinfo.num_extra_in_template;
  isMailer = cinfo.num_prev_cores > 0;
  if (isMailer)
    unique_id = linkId;
  for (int i = 0; i < MAX_GROUP_AFFINITY; i++)
    ptidFwders[i] = cinfo.prev_cores[i];

  ptidMailer = cinfo.next_cores[0];

  // printf("%d: %d %d %d %d %d %d\n", 
  //  ptid, is_da, isMailer, ptidFwders[0], ptidMailer, linkId, numGroupsPerMailer);

  #endif
  #endif

  // need to set vlen here so doesn't cause squash in vector core on change in value
  #ifdef NESTED_SIMD
  vsetvl_e32m1(NESTED_SIMD_VLEN);
  #endif


  // if (unique_id == 0) {
  //   group_info_t ginfo = get_group_info(unique_id, &tinfo);
  //   printf("ptid %d %d %d %d scalar %d %d\n", 
  //     ptid, ptidOrig_x, ptidOrig_y, ptidOrigin, ginfo.scalar_x, ginfo.scalar_y);
  // }

  asm volatile("fence\n\t");

  // int mask = getDebugMask(&cinfo);
  if (isMailer) {
    SET_PREFETCH_MASK(SCALAR_NUM_FRAMES, SCALAR_FRAME_SIZE, &start_barrier); 
  }
  else { 
    SET_PREFETCH_MASK(NUM_FRAMES, INNER_FRAME_SIZE, &start_barrier);
  }
  #else
  int mask = 0;
  #endif

  MOVE_STACK_ONTO_SCRATCHPAD();

  // do the kernel
  syrk(a, c, ptid, vtid, pdim, N, M, unique_id, total_groups, 
    mask, used, ptidMailer, isMailer, ptidFwders, numGroupsPerMailer, linkId);

  // restore stack pointer
  RECOVER_DRAM_STACK();

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

  // pthread_barrier_wait(&start_barrier);
  SET_PREFETCH_MASK(0, 0, &start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }



  return NULL;
}
