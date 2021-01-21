#ifndef __VVADD_H__
#define __VVADD_H__

// data type to do computation with
#define DTYPE float

// one of these should be defined to dictate config
// #define NO_VEC 1
// #define VEC_4_SIMD 1
// #define VEC_4_SIMD_VERTICAL 1
// #define VEC_4_SIMD_SPATIAL_UNROLLED 1
// #define VEC_4_SIMD_BIGBOI 1
// #define PACKED_SIMD 1

// in current system cacheline size is 16 so doesn't make sense to go beyond this for now
// #define VEC_16_SIMD 1
// #define VEC_16_SIMD_VERTICAL 1
// #define VEC_16_SIMD_BIGBOI 1
// #define VEC_16_SIMD_SPATIAL_UNROLLED 1

// can also input orthogonal #ifdefs
// NO_VEC/USE_VEC, VEC_4_SIMD, VEC_16_SIMD
// SPATIAL, VERTICAL, SPATIAL_UNROLL
// PREFETCH_LEN

// vvadd_execute config directives
#if !defined(NO_VEC) && !defined(PACKED_SIMD)
#define USE_VEC 1
#endif
#if defined(VEC_4_SIMD_VERTICAL) || defined(VEC_16_SIMD_VERTICAL) || defined(VEC_4_SIMD_BIGBOI) ||  defined(VEC_16_SIMD_BIGBOI)
#define VERTICAL_LOADS 1
#endif
#if defined(VEC_4_SIMD_SPATIAL_UNROLLED) || defined(VEC_16_SIMD_SPATIAL_UNROLLED)
#define SPATIAL_UNROLL 1
#endif
#if defined(VEC_4_SIMD_BIGBOI) || defined(VEC_16_SIMD_BIGBOI)
#define BIGBOI 1
#endif

// vector grouping directives
#if defined(VEC_4_SIMD) || defined(VEC_4_SIMD_BCAST) || defined(VEC_4_SIMD_VERTICAL) || defined(VEC_4_SIMD_SPATIAL_UNROLLED) || defined(VEC_4_SIMD_BIGBOI)
#define VECTOR_LEN 4
#endif
#if defined(VEC_16_SIMD) || defined(VEC_16_SIMD_VERTICAL) || defined(VEC_16_SPATIAL_UNROLLED) ||  defined(VEC_16_SIMD_BIGBOI)
#define VECTOR_LEN 16
#endif

// prefetch sizings
#define POST_REGION_WORD 512
#define INIT_FRAMES 1
#if defined(VERTICAL_LOADS) || defined(SPATIAL_UNROLL)
// load 16 words (whole cacheline at a time)
#define LOAD_LEN (CACHE_LINE_SIZE/sizeof(uint32_t))
#ifdef BIGBOI
#define LOAD_PER_CORE (LOAD_LEN / VECTOR_LEN)
#else
#define LOAD_PER_CORE (LOAD_LEN)
#endif
#define REGION_SIZE (LOAD_PER_CORE * 2)
#define NUM_REGIONS (POST_REGION_WORD / REGION_SIZE)
#elif defined(USE_VEC)
#define REGION_SIZE 2
#define NUM_REGIONS (POST_REGION_WORD / REGION_SIZE)
#endif

// define prefetch len externally
#ifdef PF
#define PREFETCH_LEN PF
// default size is the vlen
#else
#define PREFETCH_LEN VECTOR_LEN
#endif

// pthread argument for the kernel
typedef struct Kern_Args {
  DTYPE *a, *b, *c;
  int size;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *b, DTYPE *c, int size,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int size,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
