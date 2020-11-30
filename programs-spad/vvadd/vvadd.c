#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "vvadd.h"
#include "spad.h"
#include "bind_defs.h"
#include "vvadd_kernel.h"
#include "util.h"

#ifdef PACKED_SIMD
#include <riscv_vector.h>
#endif


void vvadd(DTYPE *a, DTYPE *b, DTYPE *c, int start, int end, 
    int ptid, int vtid, int dim, int unroll_len) {
  
  #ifdef PACKED_SIMD
  // based on https://github.com/riscv/rvv-intrinsic-doc/blob/master/rvv_saxpy.c

  // chunk of array to do
  int chunk = end - start;

  printf("ptid %d chunk %d\n", ptid, chunk);

  // starting array idx
  a += start;
  b += start;
  c += start;

  // declare vector integers (int) of size (32) , that all fit in one (m1) vec register
  vint32m1_t va, vb, vc;

  // stripmine over hardware vector length, l <= hw_vlen
  for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
    // NOTE
    // compiler will insert a 'setvli max_vlen' here if you dont and will mess up stripmine
    // even though seems obvious should need with the above???
    l = vsetvl_e32m1(chunk);

    // load vectors
    va = vle32_v_i32m1(a);
    vb = vle32_v_i32m1(b);

    // vector add
    vc = vadd_vv_i32m1(va, vb);

    // store vectors
    vse32_v_i32m1(c, vc);

    // increment pointers by stripmined length
    a += l;
    b += l;
    c += l;

  }

  #else
  for (int i = start + vtid; i < end; i+=unroll_len*dim) {
      DTYPE a_, b_, c_;
      a_ = a[i];
      b_ = b[i];
      // add and then store
      c_ = a_ + b_;
      // c[i] = c_;
      STORE_NOACK(c_, c + i, 0); // UH OH Not working with new compiler, others or just this? maybe a op code conflict??
  }
  #endif
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int n,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y) {
  
  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0) {
    stats_on();
  }

  // linearize tid and dim
  int ptid = ptid_x + ptid_y * pdim_x;
  int pdim = pdim_x * pdim_y;

  int start  = 0;
  int end    = 0;

  // group construction
  #ifdef VECTOR_LEN

  #if VECTOR_LEN==4
  template_info_t tinfo = init_template_4x4_2x2();
  // template_info_t tinfo = init_template_debug();
  #elif VECTOR_LEN==16
  template_info_t tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  if (cinfo.used) {
    int alignment = 16 * VECTOR_LEN;
    start = roundUp((cinfo.unique_id + 0) * n / cinfo.total_groups, alignment);
    end   = roundUp((cinfo.unique_id + 1) * n / cinfo.total_groups, alignment);
  }

  #elif !defined(USE_VEC)
  core_config_info_t cinfo = manycore_template(ptid_x, ptid_y, pdim_x, pdim_y);

  //do work division here
  start  = ( ( cinfo.unique_id + 0 ) * n ) / cinfo.total_groups;
  end    = ( ( cinfo.unique_id + 1 ) * n ) / cinfo.total_groups;
  #endif

  // construct special mask for dae example
  #ifdef USE_VEC
  int mask = getSIMDMask(&cinfo);
  #endif

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, vdim, vdim_x, vdim_y, start, end); 

  #ifdef NUM_REGIONS
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  #endif

  // only let certain tids continue
  #if defined(USE_VEC)
  if (cinfo.used == 0) return;
  #endif

  // run the actual kernel with the configuration
  #ifdef UNROLL
  int unroll_len = REGION_SIZE / 2;
  #else
  int unroll_len = 1;
  #endif

  int vtid = get_vtid(&cinfo);
  int vdim = get_vdim(&cinfo);
  int is_da = cinfo.is_scalar;

  // // for some reason 0xc22 specifically is problematic, even though have defined in gem5?
  // asm volatile ("csrw vlenb, 0\n\t");

  // move stack onto scratchpad for faster local access than default on DRAM
  MOVE_STACK_ONTO_SCRATCHPAD();

  // configure
  #ifdef USE_VEC
  tril_vvadd(mask, a, b, c, start, end, ptid, vtid, vdim, is_da);
  #else
  vvadd(a, b, c, start, end, ptid, vtid, vdim, unroll_len);
  #endif

  // restore stack pointer to DRAM
  RECOVER_DRAM_STACK();

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, DTYPE *c, int size,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->c = c;
  args->size = size;
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
  
  kernel(a->a, a->b, a->c, a->size, 
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);
      
  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  // BUG: note this printf fails if have the VECTOR_EPOCH(0), but mayber just timing thing
  // printf("ptid (%d,%d)\n", a->tid_x, a->tid_y);

  return NULL;
}
