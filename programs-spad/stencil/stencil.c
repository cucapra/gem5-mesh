#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "stencil.h"
#include "spad.h"
#include "bind_defs.h"

/*
  3x3 stencil with a single 3x3 filter
  Filter is cached in each spad.
  9 values are prefetched per frame
  Parallelize innermost loops (unrolled) so we can get away with codegen trick

  Reuse strategy - constraint is equal frame sizes and can't do remote stores to fill frames
  Each core gets 3 elements just like no reuse case but none are overlapping
  Its up to each core to do compute for those 3 elements and the two adjacents ones 
  Load x + 0, x + 1, x + 2
  Compute
  x - 1, x + 0, x + 1
         x + 0, x + 1, x + 2
                x + 1, x + 2, x + 3
  With this strategy you are the only core to have to load x + 1
  You have to reorder the data to implement! ( spaced out by strides )
  0 1 2 3 | 4 5 6 7  | 8 9 10 11 || 12 13 14 15
  0 3 6 9 | 1 4 7 10 | 2 5 8  11 || 12 15 18 21 

*/

#ifdef VERTICAL_LOADS
// number of filters done per iteration per core
#ifdef REUSE
#define CORE_STEP LOAD_DEPTH
#else
#define CORE_STEP (LOAD_DEPTH - (FILTER_DIM - 1))
#endif
#endif

inline int min(int a, int b) {
  if (a > b) {
    return b;
  }
  else {
    return a;
  }
}

#ifdef LARGE_FRAME
// having this function not inlined messing up hacky host/vec seperation
// maybe not prefetch all the way, so fill in rest or hardware frame
// this kinda sux
inline void completeHardwareFrame(int spadIdx, int *someData) {
  int remainingEntries = REGION_SIZE - (spadIdx % REGION_SIZE);
  for (int i = 0; i < remainingEntries; i++) {
    VPREFETCH_L(spadIdx, someData, 0, 4, 0);
    spadIdx++;
  }
}
#endif

#ifdef USE_VEC
void __attribute__((optimize("-fno-reorder-blocks")))
stencil_vector(
    DTYPE *a, DTYPE *b, DTYPE *c, int start_row, int end_row, int eff_cols, int ncols,
    int ptid, int vtid_x, int vtid_y, int vdim_x, int vdim_y, int mask) {

  // TODO fails if put this here... sigh
  // // how much we're actually going to do (ignore edges)
  // // TODO can we use predication instead?
  // int effCols = ncols - (FILTER_DIM - 1);

  int dim = vdim_x * vdim_y;
  int vtid = vtid_x + vtid_y * vdim_x;

  #ifdef REUSE
  int step = dim*FILTER_DIM - (FILTER_DIM - 1);
  #elif defined(VERTICAL_LOADS)
  int step = CORE_STEP*dim;
  #else
  int step = dim;
  #endif

  // TODO better way to do this for arbitrary groups
  #ifdef REUSE
  // calculate prev and next spAddr for reuse
  int *prevSpadAddr = NULL;
  int *nextSpadAddr = NULL;
  if (vtid != 0) {
    if (vtid_x == 0) prevSpadAddr = (int*)getSpAddr(ptid - (GRID_XDIM - (vdim_x - 1)), 0);
    else prevSpadAddr = (int*)getSpAddr(ptid - 1, 0);
  }
  if (vtid != dim - 1) {
    if (vtid_x == vdim_x - 1) nextSpadAddr = (int*)getSpAddr(ptid + (GRID_XDIM - (vdim_x - 1)), 0);
    else nextSpadAddr = (int*)getSpAddr(ptid + 1, 0);
  }
  // if (vtid == 1 || vtid == 3) prevSpadAddr = (int*)getSpAddr(ptid - 1, 0);
  // else if (vtid == 2) prevSpadAddr = (int*)getSpAddr(ptid - (GRID_XDIM - 1), 0); // GRID_DIM = 4 - 1 = 3
  // if (vtid == 0 || vtid == 2) nextSpadAddr = (int*)getSpAddr(ptid + 1, 0);
  // if (vtid == 1) nextSpadAddr = (int*)getSpAddr(ptid + (GRID_XDIM - 1), 0);
  #endif

  #ifdef REUSE
  int startOffset = vtid * FILTER_DIM - 1;
  #else
  int startOffset = vtid * (step/dim);
  #endif

  #ifdef LARGE_FRAME
  int frameSize = REGION_SIZE / FRAMES_PER_REGION;
  #else
  int frameSize = REGION_SIZE;
  #endif

  volatile int ohjeez = 1;
  if (ohjeez) {

  // enter vector epoch within function, b/c vector-simd can't have control flow
  VECTOR_EPOCH(mask);

  // should be a constant from static analysis of dim
  int pRatio = VECTOR_LEN / PREFETCH_LEN;

  int *spadAddr = (int*)getSpAddr(ptid, 0);
  // have r = 0 for now
  // will need broadcast to support r > 0
  int spadIdx = 0;

  ISSUE_VINST(fable0);

  // how much we're actually going to do (ignore edges)
  // TODO can we use predication instead?
  int effCols = eff_cols;

  // do initial batch of prefetching. only prefetch part of the first row
  // you need to guarentee that a full region worth of frames are in flight before issuing a block that will consume
  #ifdef LARGE_FRAME
  int prefetchFrames = FRAMES_PER_REGION;
  #else
  // arbitrary in this case
  int prefetchFrames = 4;
  #endif

  int beginCol = min(prefetchFrames * step, effCols);
  for (int r = start_row; r < start_row + 1; r++) {
    #ifdef VERTICAL_LOADS
    // exhibit temporal reuse within a frame in a cacheline (16) can do 16-2=14 3x1 filters
    // TODO spatial should also do reuse maybe between frames (by putting in temporal storage). 
    // But maybe can't do memory layout restrictions
    for (int c = 0; c < beginCol; c+=step) {
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        for (int core = 0; core < dim; core++) {
          int aIdx = (r + k1) * ncols + c + core * CORE_STEP;
          // printf("issue r %d c %d k1 %d core %d, depth %d, aIdx %d\n", r, c, k1, core, LOAD_DEPTH, aIdx);
          VPREFETCH_L(spadIdx, a + aIdx, core, LOAD_DEPTH, 1);
          VPREFETCH_R(spadIdx, a + aIdx, core, LOAD_DEPTH, 1);
        }
        spadIdx+=LOAD_DEPTH;
      }
    }
    #else
    for (int c = 0; c < beginCol; c+=step) {
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        for (int k2 = 0; k2 < FILTER_DIM; k2++) {
          int aIdx = (r + k1) * ncols + (c + k2);
          // printf("prelw sp %d r %d c %d k1 %d k2 %d idx %d\n", spadIdx, r, c, k1, k2, aIdx);
          // VPREFETCH_L(spadIdx, a + aIdx, 0, 4, 0);
          // VPREFETCH_R(spadIdx, a + aIdx, 0, 4, 0);
          for (int p = 0; p < pRatio; p++) { // NOTE unrolled b/c can statically determine pRatio is const
            VPREFETCH_L(spadIdx, a + aIdx + p * PREFETCH_LEN, p * PREFETCH_LEN, PREFETCH_LEN, 0);
            VPREFETCH_R(spadIdx, a + aIdx + p * PREFETCH_LEN, p * PREFETCH_LEN, PREFETCH_LEN, 0);
          }
          spadIdx++;
        }
      }
    }
    #endif
  }

  for (int r = start_row; r < end_row; r++) {
    int startCol = 0;
    // we've prefetch part of the first row to get ahead
    if (r == start_row) startCol = beginCol;
    #ifdef VERTICAL_LOADS
    for (int c = startCol; c < effCols; c+=step) {
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        for (int core = 0; core < dim; core++) {
          int aIdx = (r + k1) * ncols + c + core * CORE_STEP;
          // printf("mid issue r %d c %d k1 %d core %d, depth %d, aIdx %d\n", r, c, k1, core, LOAD_DEPTH, aIdx);
          VPREFETCH_L(spadIdx, a + aIdx, core, LOAD_DEPTH, 1);
          VPREFETCH_R(spadIdx, a + aIdx, core, LOAD_DEPTH, 1);
        }
        spadIdx+=LOAD_DEPTH;
      }

      if (spadIdx == POST_REGION_WORD) spadIdx = 0;

      ISSUE_VINST(fable1);

    }
    #else
    for (int c = startCol; c < effCols; c+=step) {
      // prefetch all 9 values required for computation
      #pragma GCC unroll 3
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        #pragma GCC unroll 3
        for (int k2 = 0; k2 < FILTER_DIM; k2++) {
          int aIdx = (r + k1) * ncols + (c + k2);
          
          #ifdef SINGLE_PREFETCH
          VPREFETCH_L(spadIdx, a + aIdx + 0, 0, 1);
          VPREFETCH_L(spadIdx, a + aIdx + 1, 1, 1);
          VPREFETCH_L(spadIdx, a + aIdx + 2, 2, 1);
          VPREFETCH_L(spadIdx, a + aIdx + 3, 3, 1);
          #else
          // printf("mid prelw sp %d r %d c %d k1 %d k2 %d idx %d\n", spadIdx, r, c, k1, k2, aIdx);

          for (int p = 0; p < pRatio; p++) {
            VPREFETCH_L(spadIdx, a + aIdx + p * PREFETCH_LEN, p * PREFETCH_LEN, PREFETCH_LEN, 0);
            VPREFETCH_R(spadIdx, a + aIdx + p * PREFETCH_LEN, p * PREFETCH_LEN, PREFETCH_LEN, 0);
          }
          #endif

          spadIdx++;
          
        }
      }

      // spad is circular buffer so do cheap mod here
      if (spadIdx == POST_REGION_WORD) {
        spadIdx = 0;
      }

      ISSUE_VINST(fable1);
    }
    #endif
  }

  #ifdef LARGE_FRAME
  completeHardwareFrame(spadIdx, a);
  #endif

  // issue the rest of blocks
  for (int r = start_row; r < end_row; r++) {
    // take some loads off the last row b/c already prefetched
    int colStart = effCols;
    if (r == end_row - 1) colStart = effCols - beginCol;
    for (int c = colStart; c < effCols; c+=step) {
      // printf("issue\n");
      ISSUE_VINST(fable1);
    }
  }

  // devec with unique tag
  DEVEC(devec_0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  
  return;
  }

  // vector engine code

  // declarations
  DTYPE a_, b_, c_;
  int64_t baseIdx, frameStart, spIdx; // avoids sext.w instruction when doing broadcast // TODO maybe should be doing rv32
  DTYPE *cPtr;
  int colCntr;
  DTYPE b0, b1, b2, b3, b4, b5, b6, b7, b8;
  DTYPE a0, a1, a2, a3, a4, a5, a6, a7, a8;
  int *spadAddr;
  int unmappedColLen;
  
  // entry block
  // load the full filter into spad
  fable0:
  //   #pragma GCC unroll 9
  //   for (int i = 0; i < FILTER_DIM * FILTER_DIM; i++) {
  //     spadAddr[POST_REGION_WORD + i] = b[i];
  //     // b_ = b[i]; // keep filter in regfile
  //   }
    spIdx = 0;

    cPtr = c + (start_row * (ncols-(FILTER_DIM-1))) + startOffset;
    spadAddr = (int*)getSpAddr(ptid, 0);
    colCntr = 0;
    unmappedColLen = ncols-(FILTER_DIM-1) - eff_cols;

    b0 = b[0];
    b1 = b[1];
    b2 = b[2];
    b3 = b[3];
    b4 = b[4];
    b5 = b[5];
    b6 = b[6];
    b7 = b[7];
    b8 = b[8];
  
  // loop body block
  fable1:

    // frameStart = iter * frameSize;
    // baseIdx = iter * FILTER_DIM * FILTER_DIM;
    spIdx = spIdx % POST_REGION_WORD;

    // start consumption of frame (stall unless we have the tokens we need)
    FRAME_START(frameSize);

    #ifdef REUSE

    // note we need to unroll in order to get cPtr indexing to work b/c it goes +1 +1 +3*dim
    // potentially can move routine into a function call?
    // also could access an indirection array that gives and then have counter mod 3
    // or could even have 3 seperate issue block


    int spData0 = spadAddr[spIdx + 0];
    int spData1 = spadAddr[spIdx + 1];
    int spData2 = spadAddr[spIdx + 2];
    int spData3 = spadAddr[spIdx + 3];
    int spData4 = spadAddr[spIdx + 4];
    int spData5 = spadAddr[spIdx + 5];
    int spData6 = spadAddr[spIdx + 6];
    int spData7 = spadAddr[spIdx + 7];
    int spData8 = spadAddr[spIdx + 8];

    // center computation with local values
    // important to put non-predicated first so any shared values between pred blocks
    // are not masked out... really need compiler help on this
    c_ = 0;
    c_ += b0 * spData0;
    c_ += b1 * spData1;
    c_ += b2 * spData2;
    c_ += b3 * spData3;
    c_ += b4 * spData4;
    c_ += b5 * spData5;
    c_ += b6 * spData6;
    c_ += b7 * spData7;
    c_ += b8 * spData8;
    STORE_NOACK(c_, cPtr + 1, 0);

    // if swap following two pred blocks core0 pred works, but then core3 pred doesn't work
    // definetly something wrong with pred...
    // fetch one column from the left to perform leftmost computation
    PRED_NEQ(vtid, 0);
    if (ohjeez) {
    c_ = 0;
    c_ += b0 * prevSpadAddr[spIdx + 2];
    c_ += b1 * spData0;
    c_ += b2 * spData1;
    c_ += b3 * prevSpadAddr[spIdx + 5];
    c_ += b4 * spData3;
    c_ += b5 * spData4;
    c_ += b6 * prevSpadAddr[spIdx + 8];
    c_ += b7 * spData6;
    c_ += b8 * spData7;
    STORE_NOACK(c_, cPtr, 0);
    }
    PRED_EQ(vtid, vtid);

    // fetch one column from the right to perform rightmost computation
    PRED_NEQ(vtid, dim - 1); // last core in group can't do this
    if (ohjeez) { 
    c_ = 0;
    c_ += b0 * spData1;
    c_ += b1 * spData2;
    c_ += b2 * nextSpadAddr[spIdx + 0];
    c_ += b3 * spData4;
    c_ += b4 * spData5;
    c_ += b5 * nextSpadAddr[spIdx + 3];
    c_ += b6 * spData7;
    c_ += b7 * spData8;
    c_ += b8 * nextSpadAddr[spIdx + 6];
    STORE_NOACK(c_, cPtr + 2, 0);
    }
    PRED_EQ(vtid, vtid);

    // 10 results are computed per reuse iteration
    // cPtr+=step;
    cPtr += step;
    colCntr+=step;
    CONVERGENT_IF(colCntr == eff_cols) {
      colCntr = 0;
      cPtr += unmappedColLen;
    }

    #elif defined(VERTICAL_LOADS)
    #pragma GCC unroll(14)
    for (int i = 0; i < CORE_STEP; i++) {
      c_ = 0;
      int baseSpIdx = spIdx + i;
      c_ += b0 * spadAddr[baseSpIdx + 0];
      c_ += b1 * spadAddr[baseSpIdx + 1];
      c_ += b2 * spadAddr[baseSpIdx + 2];
      c_ += b3 * spadAddr[baseSpIdx + LOAD_DEPTH + 0];
      c_ += b4 * spadAddr[baseSpIdx + LOAD_DEPTH + 1];
      c_ += b5 * spadAddr[baseSpIdx + LOAD_DEPTH + 2];
      c_ += b6 * spadAddr[baseSpIdx + 2*LOAD_DEPTH + 0];
      c_ += b7 * spadAddr[baseSpIdx + 2*LOAD_DEPTH + 1];
      c_ += b8 * spadAddr[baseSpIdx + 2*LOAD_DEPTH + 2];
      STORE_NOACK(c_, cPtr + i, 0);
    }

    cPtr += step;
    colCntr+=step;
    CONVERGENT_IF(colCntr == eff_cols) {
      colCntr = 0;
      cPtr += unmappedColLen;
    }
    #else
    // #pragma GCC unroll 9
    // for (int i = 0; i < FILTER_DIM * FILTER_DIM; i++) {
    //   LWSPEC(a_, spadAddr + i, 0);
    //   b_ = spadAddr[POST_REGION_WORD + i];
    //   c_ += a_ * b_;
    // }
    c_ = 0;
    c_ += b0 * spadAddr[spIdx + 0];
    c_ += b1 * spadAddr[spIdx + 1];
    c_ += b2 * spadAddr[spIdx + 2];
    c_ += b3 * spadAddr[spIdx + 3];
    c_ += b4 * spadAddr[spIdx + 4];
    c_ += b5 * spadAddr[spIdx + 5];
    c_ += b6 * spadAddr[spIdx + 6];
    c_ += b7 * spadAddr[spIdx + 7];
    c_ += b8 * spadAddr[spIdx + 8];

    STORE_NOACK(c_, cPtr, 0);
    // cPtr += dim;
    cPtr += dim;
    colCntr+=dim;
    CONVERGENT_IF(colCntr == eff_cols) {
      colCntr = 0;
      cPtr += unmappedColLen;
    }
    #endif

    REMEM(frameSize);

    spIdx += frameSize;
    
    // need this jump to create loop carry dependencies
    // an assembly pass will remove this instruction
    asm volatile goto("j %l[fable1]\n\t"::::fable1);

  return;
}
#endif

void /*__attribute__((optimize("-freorder-blocks-algorithm=simple"), optimize("-fno-inline"))) */
stencil_manycore(DTYPE *a, DTYPE *b, DTYPE *c, int nrows, int col_start, int col_end, int ncols, int ptid, int vtid, int dim, int row_start, int row_end) {
  for (int row = row_start; row < row_end; row++) {
    // #pragma GCC unroll 4
    for (int col = col_start; col < col_end; col++) {
      int temp = 0;
      #pragma GCC unroll 3
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        #pragma GCC unroll 3
        for (int k2 = 0; k2 < FILTER_DIM; k2++) {
          int aIdx = (row + k1) * ncols + (col + k2);
          int bIdx = k1 * FILTER_DIM + k2;
          temp += a[aIdx] * b[bIdx];
        }
      }
      int cIdx = row * (ncols-(FILTER_DIM-1)) + col;
      c[cIdx] = temp;
    }
  }
}


void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int nrows, int ncols,
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }

  // chunk across rows
  int effRows = nrows - (FILTER_DIM - 1);

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
  // template_info_t tinfo = init_template_debug();
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

  #ifdef USE_VEC
  // volatile so dont reorder this function call
  int mask = getSIMDMask(&cinfo);
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

  // move stack onto scratchpad for faster local access than default on DRAM
  MOVE_STACK_ONTO_SCRATCHPAD();

  #ifdef USE_VEC
  // do computation that we can map
  stencil_vector(a, b, c, start, end, mapped_len, ncols, ptid, vtid_x, vtid_y, vdim_x, vdim_y, mask);

  // do remainder of computation starting from offset
  stencil_manycore(a, b, c, nrows, mapped_len, mapped_len + unmapped_len, ncols, ptid, vtid, vdim, start, end);
  #else
  stencil_manycore(a, b, c, nrows, 0, mapped_len, ncols, ptid, vtid, vdim, start, end);
  #endif

  // restore stack pointer to DRAM
  RECOVER_DRAM_STACK();

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, DTYPE *c, int nrows, int ncols,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->c = c;
  args->nrows = nrows;
  args->ncols = ncols;
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
  
  kernel(a->a, a->b, a->c, a->nrows, a->ncols, 
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
