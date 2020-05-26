#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "dot.h"
#include "spad.h"
#include "../../common/bind_defs.h"

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

// to support a circular queue instead, would need to have a head and tail pointer for both producer and consumer along with data
// producer updates where head pointer is and consumer updates wehere tail pointer is (would need to inform both producer and consumer of each pointer change)

typedef struct token_queue {
  int basePtr;
  // int pairPtr;
  // word offsets in the producer and consumers scratchpad
  // int headPtrOffset;
  // int tailPtrOffset;
  // offset in scratchpad where data is stored on the consumer
  // int dataPtrOffset;
  // the max size of the queue
  int size;
  // which core we want to establish the link with
  // int otherCoreIdx;
} token_queue_t;

inline int* get_head_ptr(token_queue_t *tq, int coreId) {
  return (int*)getSpAddr(coreId, tq->basePtr + 0);
}

inline int* get_tail_ptr(token_queue_t *tq, int coreId) {
  return (int*)getSpAddr(coreId, tq->basePtr + 1);
}

inline volatile int get_head(token_queue_t *tq, int coreId) {
  return *get_head_ptr(tq, coreId);
}

inline volatile int get_tail(token_queue_t *tq, int coreId) {
  return *get_tail_ptr(tq, coreId);
}

// inline int* get_pair_tid_ptr(token_queue_t *tq, int coreId) {
//   return (int*)getSpAddr(coreId, tq->basePtr + 2);
// }

inline int* get_pair_base_ptr(token_queue_t *tq, int coreId) {
  return (int*)*(int*)getSpAddr(coreId, tq->basePtr + 2);
}

inline void set_pair_base_ptr(token_queue_t *tq, int coreId, int address) {
  *(int*)getSpAddr(coreId, tq->basePtr + 2) = address;
}

inline int* get_data_ptr(token_queue_t *tq, int coreId) {
  return (int*)getSpAddr(coreId, tq->basePtr + 3);
}

inline int* get_other_head_ptr(token_queue_t *tq, int coreId) {
  return get_pair_base_ptr(tq, coreId) + 0;
}

inline int* get_other_tail_ptr(token_queue_t *tq, int coreId) {
  return get_pair_base_ptr(tq, coreId) + 1;
}

// inline int* get_other_pair_tid_ptr(token_queue_t *tq, int coreId) {
//   return get_pair_base_ptr(tq, coreId) + 2;
// }

inline int* get_other_pair_base_ptr(token_queue_t *tq, int coreId) {
  return get_pair_base_ptr(tq, coreId) + 2; 
}

inline int* get_other_data_ptr(token_queue_t *tq, int coreId) {
  return get_pair_base_ptr(tq, coreId) + 3;
}

// want to declare token queue on the stack, so don't malloc
void init_token_queue_consumer(int spadOffset, int size, int thisCoreIdx, token_queue_t *tq) {
  tq->basePtr = spadOffset;
  tq->size = size;
  // tq->otherCoreIdx = otherCoreIdx; // confirm partner lazily, useful for vector were annoying to figure out partner

  // set head and tail offsets
  *get_head_ptr(tq, thisCoreIdx) = 0; // meaning offset 0 from base data ptr
  *get_tail_ptr(tq, thisCoreIdx) = 0;
}

void init_token_queue_producer(int spadOffset, int consumerOffset, int size, int thisCoreIdx, int otherCoreIdx, token_queue_t *tq) {
  tq->basePtr = spadOffset;
  tq->size = size;

  // *get_pair_base_ptr(tq, thisCoreIdx) = 0x10000000 | (otherCoreIdx << 12) | (consumerOffset * sizeof(uint32_t));
  // *get_other_pair_base_ptr(tq, thisCoreIdx) = 0x10000000 | (thisCoreIdx << 12) | (spadOffset * sizeof(uint32_t));
  set_pair_base_ptr(tq, thisCoreIdx, (int)(int*)getSpAddr(otherCoreIdx, consumerOffset));
  *get_other_pair_base_ptr(tq, thisCoreIdx) = (int)(int*)getSpAddr(thisCoreIdx, spadOffset);

  // printf("tid %d offset %p %p %#x %#x\n", thisCoreIdx, get_pair_base_ptr(tq, thisCoreIdx), get_other_pair_base_ptr(tq, thisCoreIdx), *get_pair_base_ptr(tq, thisCoreIdx), *get_other_pair_base_ptr(tq, thisCoreIdx));

  // tq->otherCoreIdx = otherCoreIdx;
  // *get_pair_tid_ptr(tq, thisCoreIdx) = otherCoreIdx;
  // *get_other_pair_tid_ptr(tq, otherCoreIdx) = thisCoreIdx;

  // inform other core that this will be producing for it

  // set head and tail offsets
  *get_head_ptr(tq, thisCoreIdx) = 0; // meaning offset 0 from base data ptr
  *get_tail_ptr(tq, thisCoreIdx) = 0;
}
// don't use the fancy sleepy wait that's in hammerblade but w/e
// return base offset of first to read
int wait_tokens_consumer(token_queue_t *tq, int numTokens, int coreId) {
  int numTokensAvail;
  volatile int head;
  volatile int tail;
  do {
    head = get_head(tq, coreId);
    tail = get_tail(tq, coreId);
    if (tail > head) {
      numTokensAvail = tail - head;
    }
    else if (head < tail) {
      numTokensAvail = tq->size - (head - tail);
    }
    else { // ==
      numTokensAvail = 0;
    }
    // printf("tid %d head %d tail %d tokensAvail %d tailAddr %p\n", coreId, head, tail, numTokensAvail, get_tail_ptr(tq, coreId));

  } while(numTokensAvail < numTokens);

  return head;
}

// wait for slots to be able to write to
// return base offset of first place to write to
int wait_tokens_producer(token_queue_t *tq, int numTokens, int coreId) {
  int openSpots;
  int head;
  int tail;
  do {
    int numTokensAvail;
    head = *get_head_ptr(tq, coreId);
    tail = *get_tail_ptr(tq, coreId);
    if (tail > head) {
      numTokensAvail = tail - head;
    }
    else if (head < tail) {
      numTokensAvail = tq->size - (head - tail);
    }
    else { // ==
      numTokensAvail = 0;
    }

    openSpots = tq->size - numTokensAvail;

  // } while(head + numTokens >= tail);
  } while (openSpots < numTokens);

  return tail;
}

// resolve circular offset in token queue
inline int get_circular_offset(token_queue_t *tq, int baseOffset, int tokenOffset, int bufSize) {
  int dataOffset = 0;
  int overShoot = baseOffset + tokenOffset - bufSize;
  if (overShoot > 0) {
    dataOffset = overShoot;
  }
  else {
    dataOffset = baseOffset + tokenOffset;
  }
  return dataOffset;
}

// consumer gets token
inline void *get_token(token_queue_t *tq, int tokenIdx, int coreId) {
  int head = *get_head_ptr(tq, coreId);
  int bufSize = tq->size;
  int dataOffset = get_circular_offset(tq, head, tokenIdx, bufSize);
  return (void*)(get_data_ptr(tq, coreId) + dataOffset);
}

// producer sets token
inline void set_token(token_queue_t *tq, int data, int tokenIdx, int coreId) {
  int tail = *get_other_tail_ptr(tq, coreId);
  int bufSize = tq->size;
  int dataOffset = get_circular_offset(tq, tail, tokenIdx, bufSize);
  get_other_data_ptr(tq, coreId)[dataOffset] = data;
}

// consumer consumes tokens by modifying tail pointer in both itself and producer core
void consume_tokens(token_queue_t *tq, int numTokens, int coreId) {
  int tail = *get_tail_ptr(tq, coreId);
  int bufSize = tq->size;
  int offset = get_circular_offset(tq, tail, numTokens, bufSize);

  // printf("tid %d consume tokens %d addr %p %p\n", coreId, numTokens, get_head_ptr(tq, coreId), get_other_head_ptr(tq, coreId));

  // update the pointer
  int newHeadPtr = offset;
  *get_head_ptr(tq, coreId) = newHeadPtr;
  *get_other_head_ptr(tq, coreId) = newHeadPtr; // maybe lazy ack?
}

// produce tokens by modifying head pointer both iteself and consumer core
void produce_tokens(token_queue_t *tq, int numTokens, int coreId) {
  int head = *get_head_ptr(tq, coreId);
  int bufSize = tq->size;
  int offset = get_circular_offset(tq, head, numTokens, bufSize);

  // update the pointer
  int newTailPtr = offset;
  *get_tail_ptr(tq, coreId) = newTailPtr;
  *get_other_tail_ptr(tq, coreId) = newTailPtr;

  printf("tid %d produce tokens %d new tail ptr %d == %d addr %p %p\n", 
    coreId, numTokens, *get_tail_ptr(tq, coreId), *get_other_tail_ptr(tq, coreId), 
    get_tail_ptr(tq, coreId), get_other_tail_ptr(tq, coreId));
}

// parallel reduction in a dataflow like manner using token queues
void reduce_manycore(int partialSum, DTYPE *c, int ptid, int activeTid, int dim, token_queue_t *cons0, token_queue_t *cons1, token_queue_t *prod) {
  
  // printf("tid %d vtid %d start\n", ptid, activeTid);

  int sum = partialSum;

  if (activeTid == 0) {
    // depending on size might have sligtly different end behavior
    // #ifdef NO_VEC
    // printf("tid %d vtid %d wait\n", ptid, activeTid);
    int t = wait_tokens_consumer(cons1, 1, ptid);
    int *data = (int*)get_token(cons1, t, ptid);
    sum += data[0];
    // #else
    // printf("tid %d vtid %d wait\n", ptid, activeTid);
    // int t0 = wait_tokens_consumer(cons0, 1, ptid);
    // int t1 = wait_tokens_consumer(cons1, 1, ptid);
    // printf("tid %d vtid %d consume\n", ptid, activeTid);
    // int *data0 = (int*)get_token(cons0, t0, ptid);
    // int *data1 = (int*)get_token(cons1, t1, ptid);
    // sum += data0[0] + data1[0];
    // consume_tokens(cons0, 1, ptid);
    // consume_tokens(cons1, 1, ptid);
    // #endif
    *c = sum;
    consume_tokens(cons1, 1, ptid);
  }
  else {
    // lower half consumes
    if (activeTid < dim / 2) {
      int t0 = wait_tokens_consumer(cons0, 1, ptid);
      int t1 = wait_tokens_consumer(cons1, 1, ptid);
      int *data0 = (int*)get_token(cons0, t0, ptid);
      int *data1 = (int*)get_token(cons1, t1, ptid);
      // printf("tid %d vtid %d get tokens %d %d\n", ptid, activeTid, data0[0], data1[0]);
      sum += data0[0] + data1[0];
      consume_tokens(cons0, 1, ptid);
      consume_tokens(cons1, 1, ptid);
    }

    // everyone produces, except for tid0 who does the writeback
    int tokenOffset = wait_tokens_producer(prod, 1, ptid);
    set_token(prod, sum, tokenOffset, ptid);
    produce_tokens(prod, 1, ptid);
    // printf("tid %d vtid %d produce tokens %d\n", ptid, activeTid, sum);
  }
  

  // printf("tid %d vtid %d finish\n", ptid, activeTid);

}

// based on id (in manycore its the ptid, in vector its the group id + vtid)
// figure out where to send your data to be reduced by another core
#ifdef NO_VEC
int get_reduction_dest(int src_id) {
  // pattern is to half your id and send to that id
  return src_id / 2;
}
#else
// TODO need to define this for every group size
void group_id_to_origin(int group_id, int *x, int *y) {
  if (group_id == 0) {
    *x = 1;
    *y = 0;
  }
  else if (group_id == 1) {
    *x = 0;
    *y = 2;
  }
  else if (group_id == 2) {
    *x = 2;
    *y = 2;
  }
  else {
    printf("fail bro");
  }
}

int get_ptid_from_group(int group_id, int vid_x, int vid_y, int phys_dim_x) {
  int x,y;
  group_id_to_origin(group_id, &x, &y);
  x += vid_x;
  y += vid_y;
  return y * phys_dim_x + x;
}

// a little more complicated to get when there's vector groups
int get_reduction_dest(int group_id, int vid_x, int vid_y, int virt_dim_x, int phys_dim_x, int *active_tid) {
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
  int ptid = get_ptid_from_group(dest_group_id, dest_vid_x, dest_vid_y, phys_dim_x);
  
  *active_tid = src;



  // if (!tempda) printf("tid %d group %d vid %d %d,%d src %d -- dest %d destgroups %d destvid %d %d,%d ptid %d\n", 
    // temptid, group_id, vid, vid_x, vid_y, src, dest, dest_group_id, dest_vid, dest_vid_x, dest_vid_y, ptid);

  return ptid;
}
#endif


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
  
  // // number of partial sums to expect
  // int num_partial_sums = 0;

  // group construction
  #if VECTOR_LEN==4
  // virtual group dimension
  vdim_x = 2;
  vdim_y = 2;

  int used = vector_group_template_4(ptid_x, ptid_y, pdim_x, pdim_y, 
    &vtid, &vtid_x, &vtid_y, &is_da, &orig_x, &orig_y, &master_x, &master_y, &unique_id, &total_groups);

  // TODO should use alignment
  if (used) {
    start = ( (unique_id + 0) * len ) / total_groups;
    end   = ( (unique_id + 1) * len ) / total_groups;
  }

  // num_partial_sums = total_groups * VECTOR_LEN;

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d used? %d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, 4, vdim_x, vdim_y, start, end, used); 

  #elif VECTOR_LEN==16

  vdim_x = 4;
  vdim_y = 4;

  int used = vector_group_template_16(ptid_x, ptid_y, pdim_x, pdim_y, 
    &vtid, &vtid_x, &vtid_y, &is_da, &orig_x, &orig_y, &master_x, &master_y, &unique_id, &total_groups);

  if (used) {
    start = ( (unique_id + 0) * len ) / total_groups;
    end   = ( (unique_id + 1) * len ) / total_groups;
  }

  // num_partial_sums = total_groups * VECTOR_LEN;

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d used? %d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, 16, vdim_x, vdim_y, start, end, used); 

  #elif !defined(USE_VEC)

  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  start  = ( ( ptid + 0 ) * len ) / pdim;
  end    = ( ( ptid + 1 ) * len ) / pdim;

  // num_partial_sums = pdim;

  // printf("%d->%d\n", start, end); 
  

  #endif

  // linearize some fields
  vdim = vdim_x * vdim_y;
  int orig = orig_x + orig_y * dim_x;

  #ifdef USE_VEC
  // volatile so dont reorder this function call
  int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
  #endif

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, vdim, vdim_x, vdim_y, start, end); 

  // setup token queues
  // TODO lightweight scratchpad memory allocator
  int spmOffset = 100;
  int bufSize = 10;
  int tqWords = bufSize + 4 + 2 + 2; // +2 extra just to be safe

  // each spm gets two consumer queues and one producer queue for a reduction
  token_queue_t consumer0;
  token_queue_t consumer1;
  token_queue_t producer;

  #ifdef NO_VEC
  int activeTid = ptid;
  int pairTid = get_reduction_dest(ptid); 
  #else
  int activeTid;
  int pairTid = get_reduction_dest(unique_id, vtid_x, vtid_y, vdim_x, pdim_x, &activeTid);
  #endif

  init_token_queue_consumer(spmOffset + tqWords * 0, bufSize, ptid, &consumer0);
  init_token_queue_consumer(spmOffset + tqWords * 1, bufSize, ptid, &consumer1);

  int pairOffset;
  if (activeTid % 2 == 0) {
    pairOffset = spmOffset + tqWords * 0;
  }
  else {
    pairOffset = spmOffset + tqWords * 1;
  }
  init_token_queue_producer(spmOffset + tqWords * 2, pairOffset, bufSize, ptid, pairTid, &producer);

  pthread_barrier_wait(&start_barrier);

  // if (ptid == 0)
  //   printf("size %d rated size %d mapped %d unmapped %d\n", eff_len, rated_size, mapped_len, unmapped_len);

  // only let certain tids continue
  #if defined(USE_VEC)
  if (used == 0) return;
  #endif

  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later 
  unsigned long long *spTop = getSpTop(ptid);
  // guess the remaining of the part of the frame that might be needed??
  spTop -= 12;

  unsigned long long stackLoc;
  unsigned long long temp;
  for(int i=0;i<12;i++){
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

  // accumulate partial sums locally
  #ifdef NO_VEC
  int partialSum = local_dot_manycore(a, b, start, end);
  #else
  int partialSum = local_dot_vector(a, b, vtid, start, end, mask);
  #endif

  #ifdef NO_VEC
  int active_dim = pdim;
  #else
  int active_dim = total_groups * VECTOR_LEN;
  #endif

  printf("tid %d psum %d red_tid %d activeId %d activeDim %d\n", ptid, partialSum, pairTid, activeTid, active_dim);

  // the core who does the reduction doesn't need to wait for iteself
  // num_partial_sums--;

  // // setup syncronizations
  // // TODO can we somehow manage without a barrier here?
  // int prefetchMask = (1 << PREFETCH_NUM_REGION_SHAMT) | (num_partial_sums << PREFETCH_REGION_SIZE_SHAMT);
  // PREFETCH_EPOCH(prefetchMask);

  // // make sure all cores have done this before begin kernel section --> do thread barrier for now
  // // TODO hoping for a cleaner way to do this
  // pthread_barrier_wait(&start_barrier);

  #ifdef VECTOR_LEN
  if (!is_da) // scalar cores don't have data to accumulate so should not partcipate
  #endif
  // // do reduction across cores (currently just send all to a single core rather than reduction tree)
  reduce_manycore(partialSum, c, ptid, activeTid, active_dim, &consumer0, &consumer1, &producer);


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
