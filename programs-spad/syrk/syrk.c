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

        FRAME_START(INNER_FRAME_SIZE);
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

#ifdef LONGLINES

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
    int ptid, int *fwders) {
  // chunk over vector groups. note all might not do the same amount of work
  int max_chunk_size = ceilToInt((float)N / (float)numGroups);

  // printf("%d fwders %d %d %d\n", ptid, fwders[0], fwders[1], fwders[2]);

  int flat_iter = 0;
  int sp_self = 0;
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  // printf("%d %d %d %d %d %f\n", 
    // ptid, max_chunk_size, numGroupsToSum, baseGroupId, fwders[0], ceilf((float)N/(float)numGroups));
  for (int cnt = 0; cnt < max_chunk_size; cnt++) {
    // printf("%d %d\n", ptid, cnt);
    int group_start[MAX_GROUP_AFFINITY]; 
    // figure out how many valid elements we're expecting in the frame
    int expected_elements = 0;
    for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {
      int gid = baseGroupId + g;
      if (cnt < get_group_len(gid, N, numGroups)) {
        expected_elements+=PER_CORE_MAILER_FRAME*ACCUM_GRANULARITY;
        #ifdef MAILER_PREFETCH
        expected_elements+=ACCUM_GRANULARITY;
        #endif
        group_start[g] = get_group_start(gid, N, numGroups) + cnt;
      }
      else {
        // printf("%d %d %d %d %d %d cant go\n", ptid, g, gid, N, numGroups, get_group_len(gid, N, numGroups));
        group_start[g] = -1;
      }
    }

    if (expected_elements == 0) return; // TODO not sure the issue
    for (int j = 0; j < M; j+=J_STRIDE*ACCUM_GRANULARITY) {

      #ifdef MAILER_PREFETCH
      // prefetch c into frame along with partial sums
      for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {
        int i = group_start[g];
        if (i < 0) continue;
        // place into first 3 elements
        // hard to put consectuively b/c then would need to change store pattern
        // TODO would need to change kernel code if wanted. need to alternate strides
        for (int a = 0; a < ACCUM_GRANULARITY; a++) {
          VPREFETCH_L(sp_self + g + a * SUB_FRAME_SIZE, &c[i * N + j + a], 0, 1, TO_SELF);
        }
      }
      #endif

      // inform sync
      for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {
        if (group_start[g] < 0) {
          continue;
        }
        int *sp_scalar_ptr = (int*)getSpAddr(fwders[g], 0);
        // printf("%d flat %p %d\n", ptid, sp_scalar_ptr, flat_iter);
        STORE_NOACK(flat_iter, &sp_scalar_ptr[POST_FRAME_WORD], 0); 
        // sp_scalar_ptr[POST_FRAME_WORD] = flat_iter;
      }
      flat_iter+=ACCUM_GRANULARITY;

      #ifndef MAILER_PREFETCH
      // load initial value
      DTYPE sum[MAX_GROUP_AFFINITY];
      for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {
        // #ifdef MAILER_PREFETCH
        // sum[g] = sp_ptr[g] * beta;
        // #else
        int i = group_start[g];
        if (i < 0) continue;
        sum[g] = c[i * N + j] * beta;
        // #endif
      }
      #endif

      // printf("%d start consume frame %d %d %d\n", ptid, cnt, j, expected_elements);

      // wait for frame and then do sum
      FRAME_START(expected_elements);

      // printf("%d consume %d\n", ptid, j);
      for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {
        #ifdef MAILER_PREFETCH
        #pragma GCC unroll(4)
        for (int a = 0; a < ACCUM_GRANULARITY; a++) {
          DTYPE sum = sp_ptr[sp_self + g + a * SUB_FRAME_SIZE] * beta;

          int sum_offset = MAILER_OFFSET + g * PER_CORE_MAILER_FRAME + a * SUB_FRAME_SIZE;

          #pragma GCC unroll(16)
          for (int k = 0; k < PER_CORE_MAILER_FRAME; k++) {
            sum += sp_ptr[sum_offset + sp_self + k];
          }

          int i = group_start[g];
          if (i < 0) continue;

          STORE_NOACK(sum, &c[i * N + j + a], 0);
        }
        #else
        
        #pragma GCC unroll(16)
        for (int k = 0; k < PER_CORE_MAILER_FRAME; k++) {
          sum[g] += sp_ptr[sp_self + k];
        }

        sp_self += PER_CORE_MAILER_FRAME;

        int i = group_start[g];
        if (i < 0) continue;

        STORE_NOACK(sum[g], &c[i * N + j], 0);
        #endif

      }
      #ifdef MAILER_PREFETCH
      sp_self += MAILER_FRAME_SIZE;
      #endif
      sp_self = sp_self % MAILER_POST_FRAME_WORD; // TOOD branch better??
      REMEM(expected_elements);
    }
  }
}
#endif


void __attribute__((optimize("-fno-inline"))) syrk(
    DTYPE *a, DTYPE *c,
    int ptid, int vtid, int dim, int N, int M, int groupId, int numGroups,
    int mask, int used, int ptidMailer, int isMailer, int *ptidFwder, 
    int numGroupsToSum, int linkId, int numSum, int sendOffset
  ) {

    #ifndef USE_VEC
    syrk_manycore_baseline(a, c, N, M, ptid, dim);
    #else
    if (used)
      tril_syrk(mask, a, c, N, M, ptid, groupId, numGroups, 
        vtid, ptidMailer, linkId, numGroupsToSum, numSum, sendOffset);
    #ifdef LONGLINES
    else if (isMailer)
      mailer(c, groupId, numGroups, N, M, ptid, ptidFwder);
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
  int linkId = 0;
  int isMailer = 0;
  int numSum; // how many values converge to this core during sum
  int sendOffset = 0;

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

  #ifdef LONGLINES
  #ifdef SCALAR_IS_MAILER
  int ptid_scalar_x, ptid_scalar_y;
  group_id_to_scalar(&tinfo, unique_id, &ptid_scalar_x, &ptid_scalar_y);
  int ptid_scalar = ptid_scalar_x + ptid_scalar_y * pdim_x;
  ptidMailer = ptid_scalar;
  isMailer = is_da;
  #elif defined(ROFL_COP)


  #ifdef SNAKING

  // define snaking pattern 
  // TODO define some of this stuff in templates
  // i think if define start and end , can figure out the snake pattern
  int bias_x = vtid_y % 2; // whether snaking right or left (0 == right)
  int bias_y = 0; // whether snaking down or up at ends (0 == down)

  // determine which core starts
  int starting_core_vtid, ending_core_vtid;
  // groupId = 2 (and repeating) has differing instruction flow so need to accum
  // in a different order
  #if VECTOR_LEN == 4
  // 2
  if ((unique_id + 1) % 3 == 0) {
    starting_core_vtid = 1; // i think this is always the origin vtid
    ending_core_vtid = 3; 

    // invert x bias
    bias_x = ((bias_x + 1) % 2);
  }
  // 0,1
  else {
    starting_core_vtid = 0;
    ending_core_vtid = 2;
  }
  #elif VECTOR_LEN == 16
  if (unique_id == 0) {
    starting_core_vtid = 12; // 0,3
    ending_core_vtid = 0; // 3,0
    bias_y = 1;
    bias_x = ((bias_x + 1) % 2);
  }
  else if (unique_id == 1) {
    starting_core_vtid = 15; // 7,3
    ending_core_vtid = 3; // 4,0
    bias_y = 1;
    // invert x bias
    // bias_x = ((bias_x + 1) % 2);
  }
  else if (unique_id == 2) {
    starting_core_vtid = 0;
    ending_core_vtid = 12;
  }
  #endif


  int next_x, next_y;

  // snek right
  if (bias_x == 0) {
    if (vtid_x < vdim_x-1) {
      next_y = vtid_y;
      next_x = vtid_x + 1;
    }
    else {
      if (bias_y == 0)
        next_y = vtid_y + 1;
      else
        next_y = vtid_y - 1;
      next_x = vtid_x;
    }
  }
  // snek left
  else {
    if (vtid_x > 0) {
      next_y = vtid_y;
      next_x = vtid_x - 1;
    }
    else {
      if (bias_y == 0)
        next_y = vtid_y + 1;
      else
        next_y = vtid_y - 1;
      next_x = next_x;
    }
  }

  #else
 // determine which core starts
  int starting_core_vtid, ending_core_vtid;
  // groupId = 2 (and repeating) has differing instruction flow so need to accum
  // in a different order
  #if VECTOR_LEN == 4
  // 2
  if ((unique_id + 1) % 3 == 0) {
    starting_core_vtid = 1; // i think this is always the origin vtid
    ending_core_vtid = 2; 
  }
  // 0,1
  else {
    starting_core_vtid = 0;
    ending_core_vtid = 3;
  }
  #elif VECTOR_LEN == 16
  if (unique_id == 0) {
    starting_core_vtid = 12;
    ending_core_vtid = 3;
  }
  else if (unique_id == 1) {
    starting_core_vtid = 15;
    ending_core_vtid = 0;
  }
  else if (unique_id == 2) {
    starting_core_vtid = 0;
    ending_core_vtid = 15;
  }
  #endif


  int next_x, next_y;


  // do logflow like instruction forwarding but covergent instread of divergent
  // just first and last row vectors will be different
  int start_x, start_y;
  int end_x, end_y;
  start_x = starting_core_vtid % vdim_x;
  start_y = starting_core_vtid / vdim_x;
  end_x = ending_core_vtid % vdim_x;
  end_y = ending_core_vtid / vdim_x;
  
  int dir_x = (start_x < end_x); // 1 == right
  int dir_y = (start_y < end_y); // 1 == down
  #define X_RIGHT 1
  #define Y_DOWN 1

  if (dir_x == X_RIGHT) {
    if (vtid_x == vdim_x - 1) {
      next_x = vtid_x;
      sendOffset = 1;
      if (dir_y == Y_DOWN) {
        next_y = vtid_y + 1;

        if (vtid_y == 0) numSum = 1;
        else numSum = 2;
      }
      else {
        next_y = vtid_y - 1;

        if (vtid_y == vdim_y - 1) numSum = 1;
        else numSum = 2;
      }
    }
    else {
      sendOffset = 0;
      next_x = vtid_x + 1;
      next_y = vtid_y;

      numSum = 1;
    }

    if (vtid_x == 0) numSum = 0;
  }
  else {
    if (vtid_x == 0) {
      next_x = vtid_x;
      sendOffset = 1;
      if (dir_y == Y_DOWN) {
        next_y = vtid_y + 1;

        if (vtid_y == 0) numSum = 1;
        else numSum = 2;
      }
      else {
        next_y = vtid_y - 1;

        if (vtid_y == vdim_y - 1) numSum = 1;
        else numSum = 2;
      }
    }
    else {
      sendOffset = 0;
      next_x = vtid_x - 1;
      next_y = vtid_y;

      numSum = 1;
    }

    if (vtid_x == vdim_x - 1) numSum = 0;
  }

  #endif
  // cores last in the formation will just send to self which won't count as a frame access
  if (vtid == ending_core_vtid) {
    next_x = vtid_x;
    next_y = vtid_y;
  }

  ptidMailer = get_ptid_from_group(&tinfo, unique_id, next_x, next_y, pdim_x);

  // set some fields
  linkId = starting_core_vtid;
  numGroupsPerMailer = ending_core_vtid;

  // if (unique_id == 0) {
    // printf("%d %d (%d %d) -> (%d %d) (%d %d) %d %d\n", ptid, ptidMailer, vtid_x, vtid_y, next_x, next_y, dir_x, dir_y, numSum, sendOffset);
  // }
  #else
  linkId = cinfo.link_id[0];
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
    SET_PREFETCH_MASK(MAILER_NUM_FRAMES, MAILER_FRAME_SIZE, &start_barrier); 
  }
  // else if (is_da) {
  //   SET_PREFETCH_MASK(SCALAR_NUM_FRAMES, SCALAR_FRAME_SIZE, &start_barrier);
  // }
  else { 
    SET_PREFETCH_MASK(NUM_FRAMES, INNER_FRAME_SIZE, &start_barrier);
  }
  #else
  int mask = 0;
  #endif

  MOVE_STACK_ONTO_SCRATCHPAD();

  // do the kernel
  syrk(a, c, ptid, vtid, pdim, N, M, unique_id, total_groups, 
    mask, used, ptidMailer, isMailer, ptidFwders, numGroupsPerMailer, 
    linkId, numSum, sendOffset);

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
