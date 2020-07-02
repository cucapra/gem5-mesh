#ifndef __TOKEN_QUEUE_H__
#define __TOKEN_QUEUE_H__

#include "spad.h"

// implements a token queue existing on the scratchpad

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
    else if (head > tail) {
      numTokensAvail = tq->size - (head - tail);
    }
    else { // ==
      numTokensAvail = 0;
    }
    // printf("tid %d head %d tail %d tokensAvail %d\n", coreId, head, tail, numTokensAvail);

  } while(numTokensAvail < numTokens);

  return head;
}

// wait for slots to be able to write to
// return base offset of first place to write to
int wait_tokens_producer(token_queue_t *tq, int numTokens, int coreId) {
  int openSpots;
  volatile int head;
  volatile int tail;
  do {
    int numTokensAvail;
    head = get_head(tq, coreId);
    tail = get_tail(tq, coreId);
    if (tail > head) {
      numTokensAvail = tail - head;
    }
    else if (head > tail) {
      numTokensAvail = tq->size - (head - tail);
    }
    else { // ==
      numTokensAvail = 0;
    }

    openSpots = tq->size - numTokensAvail;

    // we don't allow overlap so actually maximum size is bufSize - 1
    // TODO this seems unoptimized, but don't lose much so w/e
    openSpots--;

    // printf("tid %d head %d tail %d open spots %d\n", coreId, head, tail, openSpots);

  // } while(head + numTokens >= tail);
  } while (openSpots < numTokens);

  return tail;
}

// resolve circular offset in token queue
inline int get_circular_offset(token_queue_t *tq, int offset, int bufSize) {
  int dataOffset = 0;
  int overShoot = offset - bufSize;
  if (overShoot >= 0) {
    dataOffset = overShoot;
  }
  else {
    dataOffset = offset;
  }
  return dataOffset;
}

// consumer gets token
inline void *get_token(token_queue_t *tq, int tokenIdx, int coreId) {
  // int head = *get_head_ptr(tq, coreId);
  int bufSize = tq->size;
  int dataOffset = get_circular_offset(tq, tokenIdx, bufSize);
  // printf("tid %d get token %p\n", coreId, get_data_ptr(tq, coreId) + dataOffset);
  return (void*)(get_data_ptr(tq, coreId) + dataOffset);
}

// producer sets token
inline void set_token(token_queue_t *tq, int data, int tokenIdx, int coreId) {
  // int tail = *get_other_tail_ptr(tq, coreId);
  int bufSize = tq->size;
  int dataOffset = get_circular_offset(tq, tokenIdx, bufSize);
  // printf("tid %d set token %p\n", coreId, &(get_other_data_ptr(tq, coreId)[dataOffset]));
  get_other_data_ptr(tq, coreId)[dataOffset] = data;
}

// consumer consumes tokens by modifying tail pointer in both itself and producer core
void consume_tokens(token_queue_t *tq, int numTokens, int coreId) {
  int head = *get_head_ptr(tq, coreId);
  int bufSize = tq->size;
  int offset = get_circular_offset(tq, head + numTokens, bufSize);

  // printf("tid %d consume tokens %d addr %p %p\n", coreId, numTokens, get_head_ptr(tq, coreId), get_other_head_ptr(tq, coreId));

  // update the pointer
  int newHeadPtr = offset;
  *get_head_ptr(tq, coreId) = newHeadPtr;
  *get_other_head_ptr(tq, coreId) = newHeadPtr; // maybe lazy ack?

  // printf("tid %d consume tokens %d new head ptr %d == %d addr %p %p\n", 
  //   coreId, numTokens, *get_head_ptr(tq, coreId), *get_other_head_ptr(tq, coreId), 
  //   get_head_ptr(tq, coreId), get_other_head_ptr(tq, coreId));  
}

// produce tokens by modifying head pointer both iteself and consumer core
void produce_tokens(token_queue_t *tq, int numTokens, int coreId) {
  int tail = *get_tail_ptr(tq, coreId);
  int bufSize = tq->size;
  int offset = get_circular_offset(tq, tail + numTokens, bufSize);

  // update the pointer
  int newTailPtr = offset;
  *get_tail_ptr(tq, coreId) = newTailPtr;
  *get_other_tail_ptr(tq, coreId) = newTailPtr;

  // printf("tid %d produce tokens %d head %d new tail ptr %d == %d addr %p %p\n", 
  //   coreId, numTokens, *get_head_ptr(tq, coreId), *get_tail_ptr(tq, coreId), *get_other_tail_ptr(tq, coreId), 
  //   get_tail_ptr(tq, coreId), get_other_tail_ptr(tq, coreId));
}

#endif
