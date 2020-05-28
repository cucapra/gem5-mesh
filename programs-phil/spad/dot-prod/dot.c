#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "dot.h"
#include "spad.h"
#include "../../common/bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"

/*
  Dot product. Shows implementation of a dot product on our architecture

  The general flavor is to do local accumulation within a group/core.
  To accumulate across groups we want to go back to manycore mode
*/

inline int min(int a, int b) {
  if (a > b) {
    return b;
  }
  else {
    return a;
  }
}

int local_dot_manycore(DTYPE *a, DTYPE *b, int start, int end) {
  // accumulate a partial sum locally
  DTYPE partialSum = 0;
  for (int i = start; i < end; i++) {
    partialSum += a[i] * b[i];
  }
  return partialSum;
}

#ifdef USE_VEC
int  __attribute__((optimize("-fno-reorder-blocks")))
 local_dot_vector(DTYPE *a, DTYPE *b, int vtid, int start, int end, int mask) {

  volatile int sum = 0;

  // prevents code from being reordered :|
  volatile int ohjeez = 1;
  if (ohjeez) {

  // goto vector mode
  VECTOR_EPOCH(mask);
  
  // issue header block
  ISSUE_VINST(fable0);

  // issue loop body block
  for (int i = start; i < end; i+=VECTOR_LEN) {
    ISSUE_VINST(fable1);
  }

  // devec with unique tag
  DEVEC(devec_0);

  asm volatile("nop\n\t");

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  
  return sum;
  }

  // vector engine code

  // declarations
  DTYPE a_, b_, iter;
  DTYPE *aPtr, *bPtr;

  // header
  fable0:
    iter = 0;
    aPtr = a + start + vtid;
    bPtr = b + start + vtid;

  // body
  fable1:
    a_ = aPtr[iter];
    b_ = bPtr[iter];
    sum += a_ * b_;
    iter+=VECTOR_LEN;
    asm volatile goto("j %l[fable1]\n\t"::::fable1);
}
#endif

// parallel reduction in a dataflow like manner using token queues
void reduce_manycore(int partialSum, DTYPE *c, int ptid, int activeTid, int dim, token_queue_t *cons0, token_queue_t *cons1, token_queue_t *prod) {

  int sum = partialSum;

  // one core is responsible for writing back the final value at the end
  if (activeTid == 0) {
    // printf("tid %d atid %d dim %d special wait for %p\n", ptid, activeTid, dim, get_pair_base_ptr(cons1, ptid));
    int t = wait_tokens_consumer(cons1, 1, ptid);
    int *data = (int*)get_token(cons1, t, ptid);
    sum += data[0];
    *c += sum;
    consume_tokens(cons1, 1, ptid);
  }
  // in general cores recv two values in input token queues and writes a value to the next core
  else {
    // only lower half consumes data
    if (activeTid < dim / 2) {
      // printf("tid %d atid %d dim %d wait for %p %p\n", ptid, activeTid, dim, get_pair_base_ptr(cons0, ptid), get_pair_base_ptr(cons1, ptid));
      int t0 = wait_tokens_consumer(cons0, 1, ptid);
      int t1 = wait_tokens_consumer(cons1, 1, ptid);
      int *data0 = (int*)get_token(cons0, t0, ptid);
      int *data1 = (int*)get_token(cons1, t1, ptid);
      sum += data0[0] + data1[0];
      consume_tokens(cons0, 1, ptid);
      consume_tokens(cons1, 1, ptid);
    }

    // everyone produces, except for tid0 who does the writeback
    // printf("tid %d atid %d dim %d produce for %p\n", ptid, activeTid, dim, get_pair_base_ptr(prod, ptid));
    int tokenOffset = wait_tokens_producer(prod, 1, ptid);
    set_token(prod, sum, tokenOffset, ptid);
    produce_tokens(prod, 1, ptid);
  }

}

// based on id (in manycore its the ptid, in vector its the group id + vtid)
// figure out where to send your data to be reduced by another core
#ifndef USE_VEC
int get_reduction_dest(int src_id) {
  // pattern is to half your id and send to that id
  return src_id / 2;
}
#else
// a little more complicated to get when there's vector groups
int get_reduction_dest(template_info_t *tinfo, int group_id, int vid_x, int vid_y, int virt_dim_x, int phys_dim_x, int *active_tid) {
  // get a flat id from group and vid
  int vid = vid_y * virt_dim_x + vid_x;
  int src = group_id * VECTOR_LEN + vid;
  // divide by two as in manycore case
  int dest = src / 2;
  // get the ptid corresponding to this group
  int dest_group_id = dest / VECTOR_LEN;
  int dest_vid = dest % VECTOR_LEN;
  int dest_vid_x = dest_vid % virt_dim_x;
  int dest_vid_y = dest_vid / virt_dim_x;
  int ptid = get_ptid_from_group(tinfo, dest_group_id, dest_vid_x, dest_vid_y, phys_dim_x);
  
  *active_tid = src;

  return ptid;
}
#endif

// for some reason fails if inlined
// dot product. two parts
// 1) do local multiplication and accumulation (in vector/manycore)
// 2) do reduction always using manycore version
void __attribute__((optimize("-fno-inline"))) dot_product(DTYPE *a, DTYPE *b, DTYPE *c, 
  int start, int end, int ptid, int vtid, int activeTid, int activeDim,
  token_queue_t *consumer0, token_queue_t *consumer1, token_queue_t *producer,
  int is_da, int mask
  ) {
   // accumulate partial sums locally
  #ifndef USE_VEC
  int partialSum = local_dot_manycore(a, b, start, end);
  #else
  int partialSum = local_dot_vector(a, b, vtid, start, end, mask);
  #endif

  // printf("tid %d sum %d activeTid %d activeDim %d isDa %d pair ptr %p %p %p %p -- %p %p %p %p -- %p %p %p %p \n", ptid, partialSum, activeTid, activeDim, is_da, 
  //   get_pair_base_ptr(consumer0, ptid), get_tail_ptr(consumer0, ptid), get_tail_ptr(consumer0, ptid), get_data_ptr(consumer0, ptid),
  //   get_pair_base_ptr(consumer1, ptid), get_tail_ptr(consumer1, ptid), get_tail_ptr(consumer1, ptid), get_data_ptr(consumer1, ptid),
  //   get_pair_base_ptr(producer, ptid), get_other_head_ptr(producer, ptid), get_other_tail_ptr(producer, ptid), get_other_data_ptr(producer, ptid));

  #ifdef VECTOR_LEN
  if (!is_da) // scalar cores don't have data to accumulate so should not partcipate
  #endif
  // do reduction across cores
  reduce_manycore(partialSum, c, ptid, activeTid, activeDim, consumer0, consumer1, producer);
}


void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int len,
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
  int start  = 0;
  int end    = 0;
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

  // TODO should use alignment
  if (used) {
    start = ( (unique_id + 0) * len ) / total_groups;
    end   = ( (unique_id + 1) * len ) / total_groups;
  }

  // printf("ptid %d(%d,%d) da %d vtid %d(%d,%d) dim %d(%d,%d) %d->%d used? %d\n", ptid, ptid_x, ptid_y, is_da, vtid, vtid_x, vtid_y, 4, vdim_x, vdim_y, start, end, used);

  #elif !defined(USE_VEC)

  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  start  = ( ( ptid + 0 ) * len ) / pdim;
  end    = ( ( ptid + 1 ) * len ) / pdim;
  used   = 1;

  #endif

  // linearize some fields
  vdim = vdim_x * vdim_y;
  int orig = orig_x + orig_y * dim_x;

  // get behavior of each core
  #ifdef USE_VEC
  int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
  #else
  int mask = 0;
  #endif

  // setup token queues
  // TODO lightweight scratchpad memory allocator?
  int spmOffset = 100;
  int bufSize = 4;
  int tqWords = bufSize + 4 + 2 + 2; // +2 extra just to be safe

  // each spm gets two consumer queues and one producer queue for a reduction
  token_queue_t consumer0;
  token_queue_t consumer1;
  token_queue_t producer;

  #ifndef USE_VEC
  int activeTid = ptid;
  int pairTid = get_reduction_dest(ptid); 
  int active_dim = pdim;
  #else
  int activeTid;
  int pairTid = get_reduction_dest(&tinfo, unique_id, vtid_x, vtid_y, vdim_x, pdim_x, &activeTid);
  int active_dim = total_groups * VECTOR_LEN;
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

  // single barrier before kernel start
  pthread_barrier_wait(&start_barrier);

  // only let certain tids continue
  if (used == 0) return;

  // printf("tid %d active %d pair %d activeDim %d pair addr c0 %p c1 %p p %p\n", ptid, activeTid, pairTid, active_dim, 
  //   get_pair_base_ptr(&consumer0, ptid), get_pair_base_ptr(&consumer1, ptid), get_pair_base_ptr(&producer, ptid));

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


  // do a bunch of dot products without syncronizing in between
  for (int i = 0; i < 6; i++) {
    dot_product(a, b, c, start, end, ptid, vtid, activeTid, active_dim, &consumer0, &consumer1, &producer, is_da, mask);
  }

  // restore stack pointer
  asm volatile (
    "addi sp, %[stackTop], 0\n\t" :: [stackTop] "r" (stackLoc)
  );

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, DTYPE *c, int len,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->c = c;
  args->len = len;
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
  
  kernel(a->a, a->b, a->c, a->len,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
