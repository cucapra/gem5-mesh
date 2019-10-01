
#include "pthread_launch.h"
#include <stdlib.h>
#include <math.h>
#include <sys/sysinfo.h>
 
void launch_kernel(void* (*kernel)(void*), void **args, int cores_x, int cores_y) {
  
  // device threads will be pthreads
  // one thread will spawn on this core so don't create a pthread for that
  int num_cores = cores_x * cores_y;
  int dev_cores = num_cores - 1;
  pthread_t **threads = (pthread_t**)malloc(sizeof(pthread_t*) * dev_cores);
  
  // barrier to guarentee one thread per core (prevents from any finishing
  // before scheduling)
  pthread_barrier_init(&start_barrier, NULL, num_cores);

  // initialize pthreads
  for (int i = 0; i < dev_cores; i++) {
    threads[i] = (pthread_t*)malloc(sizeof(pthread_t));
  }

  // create a thread on each device core
  for (int i = 0; i < dev_cores; i++) {
		pthread_create(threads[i], NULL, kernel, args[i + 1]);
  }
  
  // start an iteration locally
  kernel(args[0]);

  // wait for threads to finish
  for (int i = 0; i < dev_cores; i++) {   
    pthread_join(*(threads[i]), NULL);
  }
  
  // cleanup
  for (int i = 0; i < num_cores; i++) {
    free(args[i]);
  }
  for (int i = 0; i < dev_cores; i++) {
    free(threads[i]);
  }
  free(args);
  free(threads);
  
}

int get_cores() {
  // get the number of processors in the system
  // would rather use multi-platform thread::hardware_concurreny
  // but doesn't work in c (only c++11)
  int total_cpus = get_nprocs();
  return total_cpus;
}

int get_dimensions(int *cores_x, int *cores_y) {
  int total_cpus = get_nprocs();

  // figure out device dimensions
  int width = (int)sqrt(total_cpus);
  int height = (float)total_cpus / (float)width;
  *cores_x = width;
  *cores_y = height;
  return width * height;
}
