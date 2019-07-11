
#include "pthread_launch.h"
#include <stdlib.h>
#include <math.h>
#include <sys/sysinfo.h>
 
void launch_kernel(void* (*kernel)(void*), void **args, int cores_x, int cores_y) {
  
  // device threads will be pthreads
  int num_cores = cores_x * cores_y;
  pthread_t **threads = (pthread_t**)malloc(sizeof(pthread_t*) * num_cores);
  
  // barrier to guarentee one thread per core (prevents from any finishing
  // before scheduling)
  pthread_barrier_init(&start_barrier, NULL, num_cores);

  // initialize pthreads
  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      threads[i] = (pthread_t*)malloc(sizeof(pthread_t));
    } 
  }

  // create a thread on each device core
  for (int i = 0; i < num_cores; i++) {
		pthread_create(threads[i], NULL, kernel, args[i]);
  }

  // wait for threads to finish
  for (int i = 0; i < num_cores; i++) {   
    pthread_join(*(threads[i]), NULL);
  }
  
  // cleanup
  for (int i = 0; i < num_cores; i++) {
    free(args[i]);
    free(threads[i]);
  }
  free(args);
  free(threads);
  
}

void get_dimensions(int *cores_x, int *cores_y) {
  // get the number of processors in the system
  // would rather use multi-platform thread::hardware_concurreny
  // but doesn't work in c (only c++11)
  int total_cpus = get_nprocs();

  // TODO this work is redundant with both python config and other c progs
  // use 1 cpu to do setup and monitoring (like a host)
  int device_cpus = total_cpus - 1;

  // figure out device dimensions
  int width = (int)sqrt(device_cpus);
  int height = (float)device_cpus / (float)width;
  *cores_x = width;
  *cores_y = height;
  
  
}
