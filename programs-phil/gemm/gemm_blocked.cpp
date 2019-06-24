// blocked parallel dense gemm
// blocking strategy from
// https://stackoverflow.com/questions/15829223/loop-tiling-blocking-for-large-dense-matrix-multiplication

#include <cmath>
#include "gemm_kernels.h"

using namespace std;

#ifdef DEBUG
#include <cassert>
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

inline int _idx_(int y, int x, int width) {
  return (y * width) + x;
}

inline int get_blk_end(int iter, int bound, int blk_dim) {
  int iter_next = iter + blk_dim;
  if (iter_next > bound) {
    return bound;
  }
  else {
    return iter_next;
  }
}

void *gemm(void *args)
{
  gemm_args_t *cast_args = (gemm_args_t*)args;
	int *A          = cast_args->a;
	int *B          = cast_args->b;
	int *C          = cast_args->c;
	int cid_x       = cast_args->cid_x;
  int cid_y       = cast_args->cid_y;
	int cores_x     = cast_args->cores_x;
  int cores_y     = cast_args->cores_y;
	int m           = cast_args->m;
  int n           = cast_args->n;
  int t           = cast_args->t;
  
  // TODO might want to do blocking loads here! (with volatile)
  // TODO currenlty each core is going to load in everything
  // do prefetch to emulate scratchpad
  // presumably this loads in the block of the address into the cache
  int cacheLineBytes = 64; // gem5 cache line is 64 bytes
  int cacheLineWords = cacheLineBytes / sizeof(int);
  for (int i = 0; i < m * n; i+=cacheLineWords) {
      __builtin_prefetch((const void*) (A + i));
      __builtin_prefetch((const void*) (B + i));
      __builtin_prefetch((const void*) (C + i));
  }
  //__builtin_prefetch((const void*) (A + tid));
  
  
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early
  pthread_barrier_wait(&start_barrier);

#ifdef DEBUG
  // sync outlogs
  pthread_mutex_lock(&mutex);
  cout << cid_x << ", " << cid_y << " starting." << endl;
  pthread_mutex_unlock(&mutex);
#endif
 
  // can prob quickly experiment with this using smaller gem5 caches
  int m_start = cid_y * (m / cores_y);
  int n_start = cid_x * (n / cores_x);  

  // handle untouched dimensions
  int m_chunk = (int)(m / cores_y);
  int n_chunk = (int)(n / cores_x);
  if (cid_x == cores_x - 1) {
    n_chunk += n % cores_x;
  }
  if (cid_y == cores_y - 1) {
    m_chunk += m % cores_y;
  }

  // set the blk area to be the size of a cache
  #define L1D_CACHE_BYTES 2048
  #define NUM_MATS 3
  #define L1D_CACHE_WORDS L1D_CACHE_BYTES / sizeof(int)
  #define BLK_DIM sqrt(L1D_CACHE_WORDS / NUM_MATS)
  int blk_dim = sqrt((float)L1D_CACHE_WORDS / (float)NUM_MATS);
  //int blk_dim = cacheLineWords;

#ifdef DEBUG
  cout << "BLK_DIM: " << blk_dim << endl;
#endif

  int j0_max = m_start + m_chunk;
  for (int j0 = m_start; j0 < j0_max; j0+=blk_dim) {
    
    // make sure the block doesn't go out of allocated bounds
    int j1_max = get_blk_end(j0, j0_max, blk_dim);
     
    int i0_max = n_start + n_chunk;
    for (int i0 = n_start; i0 < i0_max; i0+=blk_dim) {
      
      // make sure x direction doesnt go out of bounds
      int i1_max = get_blk_end(i0, i0_max, blk_dim);
      
      for (int k0 = 0; k0 < t; k0+=blk_dim) {
        
        int k1_max = get_blk_end(k0, t, blk_dim);
        
        // block to increase spatial locality within private cache
        // currenlty try to hit in cache line, but spads dont have lines?
        // alternively could make it the size of the entire cache
        for (int j1 = j0; j1 < j1_max; j1++) {
          
          // these inner loops are across a single cache line
          for (int i1 = i0; i1 < i1_max; i1++) {
            for (int k1 = k0; k1 < k1_max; k1++) {
              
              C[_idx_(j1, i1, n)] += A[_idx_(j1, k1, t)] * B[_idx_(k1, i1, n)];
              
            }
          }
        }
        
        
        
      }
    }
  }

#ifdef DEBUG
  // sync outlogs
  pthread_mutex_lock(&mutex);
  cout << cid_x << ", " << cid_y << " finished." << endl;
  pthread_mutex_unlock(&mutex);
#endif
  
  return NULL; 
}
