#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "vvadd.h"
#include "../../common/bind_defs.h"


#define SYNC_ADDR 1000
#define DA_SPAD 0
#define SP_INTS 512
#define NUM_REGIONS 16
#define REGION_SIZE 32

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
vvadd_execute(float *a, float *b, float *c, int start, int end, int ptid, int vtid, int dim, int unroll_len) {
  int *spAddr = getSpAddr(ptid, 0);
  
  int numRegions = NUM_REGIONS;
  int regionSize = REGION_SIZE;
  int memEpoch = 0;

  for (int i = start + vtid; i < end; i+=unroll_len*dim) {
    
    // region of spad memory we can use
    int *spAddrRegion = spAddr + (memEpoch % numRegions) * regionSize;

    for (int j = 0; j < unroll_len; j++) {

      int* spAddrA = spAddrRegion + j * 2;
      int* spAddrB = spAddrRegion + j * 2 + 1;

      float a_, b_;
      LWSPEC(a_, spAddrA, 0);
      LWSPEC(b_, spAddrB, 0);

      float c_ = a_ + b_;
      STORE_NOACK(c_, c + i + j * dim, 0);
    }
    
    // increment mem epoch to know which region to fetch mem from
    memEpoch++;

    // TODO also put REMEM here, REVEC should prob be within the inner loop to try to revec each time??
    // inform DA we are done with region, so it can start to prefetch for that region
    // if (tid == 1)
      // daeSpad[SYNC_ADDR] = memEpoch;
    spAddr[SYNC_ADDR] = memEpoch;

    // try to revec at the end of loop iteration
    REVEC(0);
    // also up the memory epoch internally
    REMEM(0);
  }
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
vvadd_access(float *a, float *b, float *c, int start, int end, int ptid, int vtid, int dim, int unroll_len, int spadCheckIdx) {
  int *spAddr = getSpAddr(ptid, 0);
  
  int numRegions = NUM_REGIONS;
  int regionSize = REGION_SIZE;

  // variable to control rate of sending
  int memEpoch = 0;

  for (int i = start; i < end; i+=unroll_len*dim) {
    // check how many regions are available for prefetch by doing a remote load
    // to master cores scratchpad to get stored epoch number there
    volatile int loadedEpoch;
    loadedEpoch = ((int*)getSpAddr(spadCheckIdx, 0))[SYNC_ADDR];
    while(memEpoch >= loadedEpoch + numRegions) {
      loadedEpoch = ((int*)getSpAddr(spadCheckIdx, 0))[SYNC_ADDR];
    }
    // printf("do epoch %d\n", memEpoch);

    // region of spad memory we can use
    int *spAddrRegion = spAddr + (memEpoch % numRegions) * regionSize;

    for (int j = 0; j < unroll_len; j++) {
      // printf("start spAddr %#x from addr %#x\n", spAddrRegion + j * 2, a + i + j * dim);
      VPREFETCH(spAddrRegion + j * 2    , a + i + j * dim, 0);
      VPREFETCH(spAddrRegion + j * 2 + 1, b + i + j * dim, 0);
    }
    // printf("complete mem epoch %d\n", memEpoch);
    memEpoch++;

    // up memory epoch in the core
    REMEM(0);

  }
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"), optimize("-fno-inline"))) 
vvadd_dae(float *a, float *b, float *c, int start, int end, 
    int ptid, int vtid, int dim, int unroll_len, int is_da, int origin) {
  if (is_da) {
    vvadd_access(a, b, c, start, end, ptid, vtid, dim, unroll_len, origin);
  }
  else {
    vvadd_execute(a, b, c, start, end, ptid, vtid, dim, unroll_len);
  }
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    float *a, float *b, float *c, int n,
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }

  // linearize tid and dim
  int tid = tid_x + tid_y * dim_x;
  int dim = dim_x * dim_y;

  // // only let certain tids continue
  // if (tid == 12) return;
  // if (tid == 9 || tid == 10 || tid == 13 || tid == 14 || tid == 15) return;
  // // if (tid == 2 || tid == 3 || tid == 6 || tid == 7 || tid == 11)  return;

  int vdim_x = 2;
  int vdim_y = 2;
  int vdim = vdim_x * vdim_y;

   // also need to change the tids to reflect position in the group
  int ptid_x = tid_x;
  int ptid_y = tid_y;
  int ptid   = tid;
  int vtid = 0;
  int start = 0;
  int end = 0;
  int origin_x = 0;
  int origin_y = 0;

  // construct 2 groups

  // group 1 top left (da == 8)
  if (ptid == 0) vtid = 0;
  if (ptid == 1) vtid = 1;
  if (ptid == 4) vtid = 2;
  if (ptid == 5) vtid = 3;
  if (ptid == 0 || ptid == 1 || ptid == 4 || ptid == 5 || ptid == 8) {
    start = 0;
    end = n / 2;
    origin_x = 0;
    origin_y = 0;
  }

  // group 2 top right (da == 11)
  if (ptid == 2) vtid = 0;
  if (ptid == 3) vtid = 1;
  if (ptid == 6) vtid = 2;
  if (ptid == 7) vtid = 3;
  if (ptid == 2 || ptid == 3 || ptid == 6 || ptid == 7 || ptid == 11) {
    start = n / 2;
    end = n;
    origin_x = 2;
    origin_y = 0;
  }

  

  // TODO group 3, bot mid

  int origin = origin_x + origin_y * dim_x;

  int vtid_x = vtid % vdim_x;
  int vtid_y = vtid / vdim_y;

  int is_da = 0;
  if (ptid == 8 || ptid == 11) {
    is_da = 1;
  }

  // construct special mask for dae example
  int mask = ALL_NORM;
  if (is_da) {
    mask = getDAEMask(origin_x, origin_y, vtid_x, vtid_y, vdim_x, vdim_y);
  }
  else {
    mask = getVecMask(origin_x, origin_y, vtid_x, vtid_y, vdim_x, vdim_y);
  }
  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) mask %d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, vdim, vdim_x, vdim_y, mask); 

  int prefetchMask = (16 << PREFETCH_NUM_REGION_SHAMT) | (32 << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);

  // make sure all cores have done this before begin kernel section --> do thread barrier for now
  // TODO hoping for a cleaner way to do this
  pthread_barrier_wait(&start_barrier);

  // only let certain tids continue
  if (tid == 12) return;
  if (tid == 9 || tid == 10 || tid == 13 || tid == 14 || tid == 15) return;

  VECTOR_EPOCH(mask);

  // run the actual kernel with the configuration
  volatile int unroll_len = 16;
  vvadd_dae(a, b, c, start, end, ptid, vtid, vdim, unroll_len, is_da, origin);

  // deconfigure
  VECTOR_EPOCH(ALL_NORM);
  
}


// helper functions
Kern_Args *construct_args(float *a, float *b, float *c, int size,
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

  return NULL;
}
