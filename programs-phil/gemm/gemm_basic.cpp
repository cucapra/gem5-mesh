#include "gemm_kernels.h"

using namespace std;

#ifdef DEBUG
#include <cassert>
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

inline int _idx_(int y, int x, int width) {
  return (y * width) + x;
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
 #ifdef DEBUG
  pthread_mutex_lock(&mutex);
  cout << cid_x << ", " << cid_y << " wait barrier." << endl;
  pthread_mutex_unlock(&mutex);
#endif
  
  
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


  for (int j = m_start; j < m_start + m_chunk; j++) { 
    for (int i = n_start; i < n_start + n_chunk; i++) {
      for (int k = 0; k < t; k++) {
        C[_idx_(j, i, n)] += A[_idx_(j, k, t)] * B[_idx_(k, i, n)];
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
