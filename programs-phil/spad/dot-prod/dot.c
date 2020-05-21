#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "dot.h"
#include "spad.h"
#include "../../common/bind_defs.h"

/*
  Dot product. Shows implementation of a dot product on our architecture
*/

inline int min(int a, int b) {
  if (a > b) {
    return b;
  }
  else {
    return a;
  }
}

void mul_manycore(DTYPE *a, DTYPE *b, DTYPE *c, int len, int tid, int dim) {
  // accumulate a partial sum locally
  int start = (tid + 0) * (len / dim);
  int end   = (tid + 1) * (len / dim);
  for (int i = 0; i < len; i++) {

  }
}




// // a shared buffer is just written once and then done
// typedef struct shared_buffer {
//   void *basePtr;
//   int size;
// } shared_buffer_t;

// // want to have token queue helpers to facilitate memory transfer for manycore code
// void init_shared_buffer_consumer(int consumerCore, int offset, int numWords, shared_buffer_t *buf) {
//   buf->basePtr = getSpAddr(consumerCore, offset);
//   buf->size = numWords;
//   int *doneFlagPtr = (int*)buf->basePtr + buf->size;
//   *doneFlagPtr = 0; // set done flag to 0
// }

// void init_shared_buffer_producer(int consumerCore, int offset, int numWords, shared_buffer_t *buf) {
//   buf->basePtr = getSpAddr(consumerCore, offset);
//   buf->size = numWords;
// }

// // consumer tries to read token queue, only return ptr when data is ready
// void *consume_shared_buffer(shared_buffer_t *buf) {
//   int *doneFlagPtr = (int*)buf->basePtr + buf->size;
//   while (*doneFlagPtr != 1) {}

//   return buf->basePtr;
// }

// // producer pushes to consumer once ready
// void produce_shared_buffer(shared_buffer_t *buf, void *mem, int numWords) {
//   for (int i = 0; i < numWords; i++) {
//     ((int*)(buf->basePtr))[i] = ((int*)(mem))[i];
//   }
//   int *doneFlagPtr = (int*)buf->basePtr + buf->size;
//   *doneFlagPtr = 1;
// }

// to support a circular queue instead, would need to have a head and tail pointer for both producer and consumer along with data
// producer updates where head pointer is and consumer updates wehere tail pointer is (would need to inform both producer and consumer of each pointer change)

// typedef struct token_queue {
//   // word offsets in the producer and consumers scratchpad
//   int headPtrOffset;
//   int tailPtrOffset;
//   // offset in scratchpad where data is stored on the consumer
//   int dataPtrOffset;
//   // the max size of the queue
//   int size;
//   // which core we want to establish the link with
//   int otherCoreIdx;
// } token_queue_t;

// // want to declare token queue on the stack, so don't malloc
// token_queue_t create_token_queue(int spadOffset, int size, int otherCoreIdx) {
//   token_queue_t tq;
//   tq.headPtrOffset = spadOffset + 0;
//   tq.tailPtrOffset = spadOffset + 1;
//   tq.dataPtrOffset = spadOffset + 2;
//   tq.size = size;
//   tq.otherCoreIdx = otherCoreIdx;
// }

// // don't use the fancy sleepy wait that's in hammerblade but w/e
// void wait_tokens(token_queue_t *tq, int numTokens) {
//   int numTokensAvail;
//   do {
//     int head = *(int*)getSpAddr(0, tq->headPtrOffset);
//     int tail = *(int*)getSpAddr(0, tq->tailPtrOffset);
//     if (head > tail) {
//       numTokensAvail = head - tail;
//     }
//     else {
//       numTokensAvail = tq->size - (tail - head);
//     }

//   } while(numTokensAvail < numTokensAvail);
// }

// void *get_token(token_queue_t *tq, int numTokens) {

// }


// do a reduction, which cores to accumulate in? maybe for now just accumulate in a single core
// it won't matter for the grid size we're doing most likely (64 threads, might matter if have 1000s like in a GPU)
void reduce_manycore(DTYPE *a, DTYPE *b, DTYPE *c, int len, int tid, int dim) {
  
  // advantage of remote loads is that don't need to have sync buffers
  // disadvantage is that need to figure out which cores have data which is not trivial in vector core case

  // would be nice if template had an easy to get all active vector cores
  

  // don't do a parallel reduction tree, instead just have one core do the summation of 64 values
  // instead of having to wait for remote store from each core, can just load from them manually
  if (tid == 0) {

  }
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

  // group construction
  #if VECTOR_LEN==4
  // virtual group dimension
  vdim_x = 2;
  vdim_y = 2;

  int used = vector_group_template_4(ptid_x, ptid_y, pdim_x, pdim_y, 
    &vtid, &vtid_x, &vtid_y, &is_da, &orig_x, &orig_y, &master_x, &master_y, &unique_id, &total_groups);

  // TODO should use alignment
  if (used) {
    start = ( (unique_id + 0) * effRows ) / total_groups;
    end   = ( (unique_id + 1) * effRows ) / total_groups;
  }

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d used? %d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, 4, vdim_x, vdim_y, start, end, used); 

  #elif VECTOR_LEN==16

  vdim_x = 4;
  vdim_y = 4;

  int used = vector_group_template_16(ptid_x, ptid_y, pdim_x, pdim_y, 
    &vtid, &vtid_x, &vtid_y, &is_da, &orig_x, &orig_y, &master_x, &master_y, &unique_id, &total_groups);

  if (used) {
    start = ( (unique_id + 0) * effRows ) / total_groups;
    end   = ( (unique_id + 1) * effRows ) / total_groups;
  }

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d used? %d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, 16, vdim_x, vdim_y, start, end, used); 

  #elif !defined(USE_VEC)

  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  start  = ( ( ptid + 0 ) * effRows ) / pdim;
  end    = ( ( ptid + 1 ) * effRows ) / pdim;

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

  #ifdef NUM_REGIONS
  int prefetchMask = (NUM_REGIONS << PREFETCH_NUM_REGION_SHAMT) | (REGION_SIZE << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);

  // make sure all cores have done this before begin kernel section --> do thread barrier for now
  // TODO hoping for a cleaner way to do this
  pthread_barrier_wait(&start_barrier);
  #endif

  // each vector group size is rated to do a certain problem size and multiples of that problem size
  // for the mod of this we need to do the rest on the flexible manycore version
  int rated_size = 0;
  #ifdef REUSE
  rated_size = ( VECTOR_LEN * FILTER_DIM - (FILTER_DIM - 1) );
  #elif defined(VERTICAL_LOADS)
  rated_size = ( VECTOR_LEN * CORE_STEP );
  #elif defined(VECTOR_LEN)
  rated_size = ( VECTOR_LEN * FILTER_DIM );
  #else
  rated_size = 1;
  #endif

  // cols without the edge case
  int eff_len = ncols - (FILTER_DIM - 1);
  // mapped len is schedule on main config, unmapped will be scheduled on base manycore
  int unmapped_len = eff_len % rated_size;
  int mapped_len = eff_len - unmapped_len;

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
  asm volatile (
    // copy part of the stack onto the scratchpad in case there are any loads to scratchpad right before
    // function call
    "ld t0, 0(sp)\n\t"
    "sd t0, 0(%[spad])\n\t"
    "ld t0, 8(sp)\n\t"
    "sd t0, 8(%[spad])\n\t"
    "ld t0, 16(sp)\n\t"
    "sd t0, 16(%[spad])\n\t"
    "ld t0, 24(sp)\n\t"
    "sd t0, 24(%[spad])\n\t"
    "ld t0, 32(sp)\n\t"
    "sd t0, 32(%[spad])\n\t"
    "ld t0, 40(sp)\n\t"
    "sd t0, 40(%[spad])\n\t"
    "ld t0, 48(sp)\n\t"
    "sd t0, 48(%[spad])\n\t"
    "ld t0, 56(sp)\n\t"
    "sd t0, 56(%[spad])\n\t"
    "ld t0, 64(sp)\n\t"
    "sd t0, 64(%[spad])\n\t"
    "ld t0, 72(sp)\n\t"
    "sd t0, 72(%[spad])\n\t"
    "ld t0, 80(sp)\n\t"
    "sd t0, 80(%[spad])\n\t"
    "ld t0, 88(sp)\n\t"
    "sd t0, 88(%[spad])\n\t"
    // save the stack ptr
    "addi %[dest], sp, 0\n\t" 
    // overwrite stack ptr
    "addi sp, %[spad], 0\n\t"
    : [dest] "=r" (stackLoc)
    : [spad] "r" (spTop)
  );

  #ifdef USE_VEC
  // do computation that we can map
  stencil_vector(a, b, c, start, end, mapped_len, ncols, ptid, vtid_x, vtid_y, vdim_x, vdim_y, mask);

  // do remainder of computation starting from offset
  stencil_manycore(a, b, c, nrows, mapped_len, mapped_len + unmapped_len, ncols, ptid, vtid, vdim, start, end);
  #else
  stencil_manycore(a, b, c, nrows, 0, mapped_len, ncols, ptid, vtid, vdim, start, end);
  #endif

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
