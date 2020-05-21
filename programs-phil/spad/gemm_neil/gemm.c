#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "gemm.h"
#include "spad.h"
#include "../../common/bind_defs.h"

#include "gemm_kernel.h"


// #define SIMD_PRIVATE_16
// #define SIMD_SHARING_16
// #define NO_VEC

// #define VEC_LEN_16
// #define VEC_LEN_4

#ifdef SIMD_SHARING_4
#define _VEC
#define USE_VECTOR_SIMD
#define SHARING
#define VEC_LEN_4
#define DIM_X 2
#endif
#ifdef SIMD_SHARING_16
#define _VEC
#define USE_VECTOR_SIMD
#define SHARING
#define VEC_LEN_16
#define DIM_X 4
#endif
#ifdef SIMD_PRIVATE_4
#define _VEC
#define USE_VECTOR_SIMD
#define VEC_LEN_4
#endif
#ifdef SIMD_PRIVATE_16
#define _VEC
#define USE_VECTOR_SIMD
#define VEC_LEN_16
#endif
#ifdef NO_VEC
#define IN_SPAD
#define OUT_SPAD
#define INTERLEAVED
#endif

// #ifdef USE_VECTOR_SIMD
// #define VPF
// #endif

//#define SHARING
#ifdef VPF
#define VEC_PFETCH
#define OUT_SPAD
#define IN_SPAD
#elif defined SP
#define OUT_SPAD
#define IN_SPAD
#else
#endif

#define BLK_DIM 4

#ifdef SHARING
#define REGION_SIZE (BLK_DIM*2)/DIM_X
#define NUM_REGIONS (512 / REGION_SIZE)
#else
#define REGION_SIZE (BLK_DIM * 2)
#define NUM_REGIONS (512 / REGION_SIZE)
#endif

//#define _VEC
//#define UNBLOCKED_INNER
//#define BLOCKED

static inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

static inline int get_blk_end(int iter, int bound, int blk_dim)
{
  int iter_next = iter + blk_dim;
  if (iter_next > bound)
  {
    return bound;
  }
  else
  {
    return iter_next;
  }
}

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
gemm(DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
     int m_start, int m_end, int n_start, int n_end, int tid)
{

#if defined(UNBLOCKED_INNER)
  // use inner product unblocked gemm - minimize DRAM writes in this case
  for (int i = m_start; i < m_end; i++)
  {
    for (int j = n_start; j < n_end; j++)
    {
      DTYPE c_temp = 0;
      for (int k = 0; k < t; k++)
      {
        c_temp += a[_idx_(i, k, t)] * b[_idx_(k, j, n)];
      }
      c[_idx_(i, j, n)] = c_temp;
    }
  }

#elif defined(UNBLOCKED_OUTER)
  for (int k = 0; k < t; k++)
  {
    for (int i = m_start; i < m_end; i++)
    {
      for (int j = n_start; j < n_end; j++)
      {
        c[_idx_(i, j, n)] += a[_idx_(i, k, t)] * b[_idx_(k, j, n)];
      }
    }
  }

#else

  int spadRegion = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(tid, 0);

#ifdef OUT_SPAD
  DTYPE *sp_c = spAddr + NUM_REGIONS * REGION_SIZE;
#endif

  int offset_x, offset_y;
  const int dim_x = 2; //num cpu in a group in x dim
  const int dim_y = 2;

#if defined(BLOCKED)
  // blocked would mean each core processes its tile in blocks to fit on scratchpad
  offset_x = offset_y = BLK_DIM;
#elif defined(INTERLEAVED)
  offset_x = BLK_DIM * dim_x;
  offset_y = BLK_DIM * dim_y;
#endif

  //assuming m_start-m_end is divisble by BLK_DIM
  for (int i0 = m_start; i0 < m_end; i0 += offset_x)
  {
    for (int j0 = n_start; j0 < n_end; j0 += offset_y)
    {
      for (int k = 0; k < t; k++)
      {

#ifdef IN_SPAD

        DTYPE *sp_a = spAddr + spadRegion * REGION_SIZE + 0;
        DTYPE *sp_b = spAddr + spadRegion * REGION_SIZE + BLK_DIM;

        // fetch a in scratchpad
        for (int i = 0; i < BLK_DIM; i++)
        {
#ifdef VEC_PFETCH
          VPREFETCH(sp_a + i, a + _idx_(i + i0, k, t), 0);
#else
          sp_a[i] = a[_idx_(i + i0, k, t)];
#endif
        }

        // fetch b in scratchpad
        for (int j = 0; j < BLK_DIM; j++)
        {
#ifdef VEC_PFETCH
          VPREFETCH(sp_b + j, b + _idx_(k, j + j0, n), 0);
#else
          sp_b[j] = b[_idx_(k, j + j0, n)];
#endif
        }

#endif

        for (int i = 0; i < BLK_DIM; i++)
        {
          for (int j = 0; j < BLK_DIM; j++)
          {
            DTYPE a_, b_;
#ifdef IN_SPAD
            a_ = sp_a[i];
            b_ = sp_b[j];
#else
            a_ = a[_idx_(i + i0, k, t)];
            b_ = b[_idx_(k, j + j0, n)];
#endif
#ifdef OUT_SPAD
            sp_c[_idx_(i, j, BLK_DIM)] += a_ * b_;
#else
            c[_idx_(i + i0, j + j0, n)] += a_ * b_;
#endif
          }
        }

#ifdef VEC_PFETCH
        spadRegion = (spadRegion + 1) % NUM_REGIONS;
        REMEM(0);
#endif
      }

//store c in DRAM
#ifdef OUT_SPAD
      for (int i = 0; i < BLK_DIM; i++)
      {
        for (int j = 0; j < BLK_DIM; j++)
        {
          STORE_NOACK(sp_c[_idx_(i, j, BLK_DIM)], c + _idx_(i + i0, j + j0, n), 0);
          sp_c[_idx_(i, j, BLK_DIM)] = 0;
        }
      }
#endif
    }
  }

#endif
}

// actual kernel
void kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
    int tid_x, int tid_y, int dim_x, int dim_y)
{

  // start recording all stats (all cores)
  // use the last thread, b/c this wakes up last?
  if (tid_x == 0 && tid_y == 0)
  {
    stats_on();
  }

#if !defined _VEC
#if defined INTERLEAVED
  int m_start = tid_y * BLK_DIM;
  int n_start = tid_x * BLK_DIM;

  int m_end = m - BLK_DIM * (dim_y - tid_y - 1);
  int n_end = n - BLK_DIM * (dim_x - tid_x - 1);
#else
  int m_start = tid_y * (m / dim_y);
  int n_start = tid_x * (n / dim_x);

  // get end with remainders
  int m_chunk = (int)(m / dim_y);
  int n_chunk = (int)(n / dim_x);
  if (tid_x == dim_x - 1)
  {
    n_chunk += n % dim_x;
  }
  if (tid_y == dim_y - 1)
  {
    m_chunk += m % dim_y;
  }
  int m_end = m_start + m_chunk;
  int n_end = n_start + n_chunk;
#endif
#endif


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

#if defined USE_VECTOR_SIMD

  #ifdef VEC_LEN_4
  // vec len 4 currently
 // virtual group dimension
  vdim_x = 2;
  vdim_y = 2;
  vdim = vdim_x * vdim_y;

  int ptid_group_[4];

  int used = vector_group_template_4(ptid_x, ptid_y, pdim_x, pdim_y,
    &vtid, &vtid_x, &vtid_y, &is_da, &orig_x, &orig_y, &master_x, &master_y, &unique_id, &total_groups);

  if(used && is_da==0){
    
    //-----------og_ptid-------v2_ptid------------//
    //             |              |
    //             |              |
    //-----------v3_ptid--------v4_ptid------------//
    //linearize origin vector ptid
    int og_ptid = orig_x + orig_y*dim_x;
    int v2_ptid = og_ptid+1;
    int v3_ptid = og_ptid+dim_x;
    int v4_ptid = v3_ptid+1;

    ptid_group_[0]=og_ptid;
    ptid_group_[1]=v2_ptid;
    ptid_group_[2]=v3_ptid;
    ptid_group_[3]=v4_ptid;
  }
  
  int m_start=0;
  int n_start = 0;
  int m_end=m;
  int n_end = n;

  if (used) {
    int alignment = BLK_DIM * vdim_x;
    // divide 'm' among groups, each groups works on [0,n) for output matrix mxn
    m_start = roundUp((unique_id + 0) * m / total_groups, alignment); 
    m_end   = roundUp((unique_id + 1) * m / total_groups, alignment); //TODO:m_end can be greater than m in cases of m%alignment !=0
  }

  if (used == 0) return;
  if (m_start == m_end) return;

  // if (unique_id>0) return; //only keep the 1st group for now
  #elif defined VEC_LEN_16
  vdim_x = 4;
  vdim_y = 4;
  vdim = vdim_x * vdim_y;

  int ptid_group_[16];

  int used = vector_group_template_16(ptid_x, ptid_y, pdim_x, pdim_y,
    &vtid, &vtid_x, &vtid_y, &is_da, &orig_x, &orig_y, &master_x, &master_y, &unique_id, &total_groups);

  
  int m_start=0;
  int n_start = 0;
  int m_end=m;
  int n_end = n;

  if (used) {
    int alignment = BLK_DIM * vdim_x;
    // divide 'm' among groups, each groups works on [0,n) for output matrix mxn
    m_start = roundUp((unique_id + 0) * m / total_groups, alignment); 
    m_end   = roundUp((unique_id + 1) * m / total_groups, alignment); //TODO:m_end can be greater than m in cases of m%alignment !=0
  }
  else return;

  if (m_start == m_end) return;

  if(used && is_da==0){
    int og_ptid = orig_x + orig_y*dim_x;
    for(int i=0; i<vdim_y;i++){
      for(int j=0; j<vdim_x; j++){
        ptid_group_[i*vdim_x+j]=og_ptid;
        og_ptid++;
      }
      og_ptid+=dim_x-vdim_x;
    }
  }
  #endif
#else
  vdim_x = 1;
  vdim_y = 1;
  vdim   = vdim_x * vdim_y;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  orig_x = 0;
  orig_y = 0; //only have 1 group for now
#endif

#ifdef _VEC
  int prefetchMask = (NUM_REGIONS << PREFETCH_NUM_REGION_SHAMT) | (REGION_SIZE << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);
#endif
  //pthread_barrier_wait(&start_barrier);

#ifdef USE_VECTOR_SIMD
  int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
#elif defined _VEC
  int mask = getVecMask(orig_x, orig_y, tid_x, tid_y, dim_x, dim_y);
#elif defined NO_VEC
  int mask = 0;
#endif


  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later

  unsigned long long *spTop = getSpTop(ptid);
  // // guess the remaining of the part of the frame that might be needed??
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
  gemm_vec_simd(mask, a, b, c, m, n, t, m_start, m_end, n_start, n_end, vtid_x, vtid_y, vtid, ptid, ptid_group_);
#elif defined _VEC
  //configure
  VECTOR_EPOCH(mask);
#if defined VPF
  gemm_vec(a, b, c, m, n, t, m_start, m_end, n_start, n_end, tid);
#else
  gemm(a, b, c, m, n, t, m_start, m_end, n_start, n_end, tid); // for non VPF case, algorithm is same - use same kernel to compare baseline and vec config
#endif

  //pthread_barrier_wait(&start_barrier);
  // deconfigure
  VECTOR_EPOCH(0);

#else
  gemm(a, b, c, m, n, t, m_start, m_end, n_start, n_end, ptid);
#endif

  // restore stack pointer
  asm volatile(
      "addi sp, %[stackTop], 0\n\t" ::[stackTop] "r"(stackLoc));
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->b = b;
  args->c = c;
  args->m = m;
  args->n = n;
  args->t = t;
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

  kernel(a->a, a->b, a->c, a->m, a->n, a->t,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  if (a->tid_x == 1 && a->tid_y == 1)
  {
    stats_off();
  }

  return NULL;
}
