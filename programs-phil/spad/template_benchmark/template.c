#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "template.h"
#include "spad.h"
#include "../../common/bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"

#include "template_kernel.h"

// use this to chunk data among vector groups
// https://stackoverflow.com/questions/3407012/c-rounding-up-to-the-nearest-multiple-of-a-number
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

void __attribute__((optimize("-fno-inline")))
template_manycore()
{

}


void kernel(DTYPE *a, int n,
    int tid_x, int tid_y, int dim_x, int dim_y)
{

  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0)
  {
    stats_on();
  }


  // linearize tid and dim
  int tid = tid_x + tid_y * dim_x;
  int dim = dim_x * dim_y;

  // split into physical and virtual tids + dim
  int ptid_x = tid_x;
  int ptid_y = tid_y;
  int ptid = tid;
  int pdim_x = dim_x;
  int pdim_y = dim_y;
  int pdim = dim;
  int vtid_x = 0;
  int vtid_y = 0;
  int vtid = 0;
  int vdim_x = 0;
  int vdim_y = 0;
  int vdim = 0;
  int start = 0;
  int end = 0;
  int orig_x = 0;
  int orig_y = 0;
  int is_da = 0;
  int master_x = 0;
  int master_y = 0;
  int unique_id = 0;
  int total_groups = 0;
  int used=0;


  #ifdef _VEC
  #if VEC_LEN==4
  template_info_t tinfo = init_template_4x4_2x2();
  #elif VEC_LEN==16
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

  
  if(used){
    //do work division here
    int alignment = VEC_LEN; //each group should have elements of multiple of this number
    start = roundUp((unique_id + 0) * n / total_groups, alignment); 
    end = roundUp((unique_id + 1) * n / total_groups, alignment); 
  }

  #else
  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  //do work division here
  start  = ( ( ptid + 0 ) * n ) / pdim;
  end    = ( ( ptid + 1 ) * n ) / pdim;
  used = 1;
  #endif


// linearize some fields
  vdim = vdim_x * vdim_y;
  int orig = orig_x + orig_y * dim_x;

  // get behavior of each core
  #ifdef _VEC
  int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
  #else
  int mask = 0;
  #endif


  // setup token queues
  // TODO lightweight scratchpad memory allocator?
  #ifndef _VEC
  int spmOffset = 100; //Sp offset for tokens
  #else
  int spmOffset = REGION_SIZE*NUM_REGIONS; //keep it in non region area
  #endif
  int bufSize = 4;
  int tqWords = bufSize + 4 + 2 + 2; // +2 extra just to be safe

  // each spm gets two consumer queues and one producer queue for a reduction
  token_queue_t consumer0;
  token_queue_t consumer1;
  token_queue_t producer;

  #ifndef _VEC
  int activeTid = ptid;
  int pairTid = get_reduction_dest(ptid); 
  int active_dim = pdim;
  #else
  int activeTid;
  int pairTid = get_reduction_dest(&tinfo, unique_id, vtid_x, vtid_y, vdim_x, pdim_x, &activeTid);
  int active_dim = total_groups * VEC_LEN;
  #endif

  init_token_queue_consumer(spmOffset + tqWords * 0, bufSize, ptid, &consumer0);
  init_token_queue_consumer(spmOffset + tqWords * 1, bufSize, ptid, &consumer1);

  // important to skip this if the core won't be used b/c might overwrite the link ptr
  if (used && !is_da) {
    // figure out which token queue you're going to be sending to
    int pairOffset;
    if (activeTid % 2 == 0) {
      pairOffset = spmOffset + tqWords * 0;
    }
    else {
      pairOffset = spmOffset + tqWords * 1;
    }
    init_token_queue_producer(spmOffset + tqWords * 2, pairOffset, bufSize, ptid, pairTid, &producer);
  }



  // region based mask for scratchpad
#ifdef _VEC
  int prefetchMask = (NUM_REGIONS << PREFETCH_NUM_REGION_SHAMT) | (REGION_SIZE << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);
#endif

// only let certain tids continue
  if (used == 0) return;


  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later

  unsigned long long *spTop = getSpTop(ptid);
  // // guess the remaining of the part of the frame (n) that might be needed?? here n = 30
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


#if defined USE_VECTOR_SIMD
  template_vec(mask);

#else
  template_manycore();
#endif

  // restore stack pointer
  asm volatile(
      "addi sp, %[stackTop], 0\n\t" ::[stackTop] "r"(stackLoc));
}

// helper functions
Kern_Args *construct_args(DTYPE *a, int n,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->n = n;
  args->tid_x = tid_x;
  args->tid_y = tid_y;
  args->dim_x = dim_x;
  args->dim_y = dim_y;

  return args;
}

void *pthread_kernel(void *args)
{
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early

  pthread_barrier_wait(&start_barrier);

  // call the spmd kernel
  Kern_Args *a = (Kern_Args *)args;

  kernel(a->a, a->n,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
