#include <thread>
#include <cmath>

#include "gemm_kernels.h"

using namespace std;

#ifdef DEBUG
#include <cassert>
#endif

pthread_barrier_t start_barrier;

inline int _idx_(int y, int x, int width) {
  return (y * width) + x;
}

int main(int argc, char *argv[])
{
  unsigned num_values;
  if (argc == 1) {
      num_values = 64; // leads to 32*32*4bytes*3 mats = ~12kB
  } else if (argc == 2) {
      num_values = atoi(argv[1]);
      if (num_values <= 0) {
          cerr << "Usage: " << argv[0] << " [num_values]" << endl;
          return 1;
      }
  } else {
      cerr << "Usage: " << argv[0] << " [num_values]" << endl;
      return 1;
  }

  unsigned totalCpus = thread::hardware_concurrency();

  // use 1 cpu to do setup and monitoring (like a host)
  unsigned deviceCpus = totalCpus - 1;

#ifdef DEBUG
  cout << totalCpus << " device cores exist. " << endl;
#endif

  int m = num_values;
  int n = num_values;
  int t = num_values;

  // don't use more cores than the problem size
  if (deviceCpus > m * n) deviceCpus = m * n;

#ifdef DEBUG
  cout << "but only using " << deviceCpus << " device cores. ";
#endif

  int *a, *b, *c;
  a = new int[m * t];
  b = new int[t * n];
  c = new int[m * n];

  if (!(a && b && c)) {
      cerr << "Allocation error!" << endl;
      return 2;
  }

  // init data in matrices
  for (int j = 0; j < m; j++) {
    for (int k = 0; k < t; k++) {
      a[_idx_(j, k, m)] = 1;
    }
  }
  
  for (int k = 0; k < t; k++) {
    for (int i = 0; i < n; i++) {
      b[_idx_(k, i, t)] = 1;
    }
  }
  
  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      c[_idx_(j, i, m)] = 0;
    }
  }

  // figure out device dimensions
  int width = (int)sqrt(deviceCpus);
  int height = (float)deviceCpus / (float)width;
  int cores_x = width;
  int cores_y = height;
  int num_cores = cores_x * cores_y;

#ifdef DEBUG
  cout << "Running on " << cores_x << " x " << cores_y << " cores. ";
  cout << "with " << num_values << " values" << endl;
#endif


  // device threads will be pthreads
  pthread_t **threads = new pthread_t*[num_cores];
  
  // barrier to guarentee one thread per core (prevents from any finishing
  // before scheduling)
  pthread_barrier_init(&start_barrier, NULL, num_cores);

  // initialize the arguments to send to each device core
  gemm_args_t **gemm_args = new gemm_args_t*[num_cores];

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = _idx_(y, x, cores_x);
      threads[i] = new pthread_t();
   
      gemm_args[i] = new gemm_args_t(a, b, c, x, y, cores_x, cores_y, m, n, t);
    }  
  }

  // create a thread on each device core
  for (int i = 0; i < num_cores; i++) {
    void *args = (void*)(gemm_args[i]);
		pthread_create(threads[i], NULL, gemm, args);
  }
#ifdef DEBUG
  cout << "Waiting for other threads to complete" << endl;
#endif
  for (int i = 0; i < num_cores; i++) {
#ifdef DEBUG
      assert(threads[i] != NULL);
#endif      
    pthread_join(*(threads[i]), NULL);
  }

  delete[] threads;
  delete[] gemm_args;
  
  pthread_barrier_destroy(&start_barrier);
#ifdef DEBUG
  cout << "Validating..." << flush;

  int num_elements = m * n;
  int num_valid = 0;
  for (int i = 0; i < num_elements; i++) {
      if (c[i] == t) {
          num_valid++;
      } else {
          cerr << "c[" << i << "] is wrong.";
          cerr << " Expected " << num_elements;
          cerr << " Got " << c[i] << "." << endl;
      }
  }

  if (num_valid == num_elements) {
      cout << "Success!" << endl;
      return 0;
  } else {
      return 2;
  }
#else
  return 0;
#endif
}
