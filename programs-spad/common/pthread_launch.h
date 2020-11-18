#ifndef __PTHREAD_LAUNCH_H__
#define __PTHREAD_LAUNCH_H__

/*
 * Launches the specified number of pthreads with the specified number
 * of args
 */ 

#include <pthread.h>

extern pthread_barrier_t start_barrier;

void launch_kernel(void* (*func)(void*), void **args, int cores_x, int cores_y);
int get_cores();
int get_dimensions(int *cores_x, int *cores_y);
void *malloc_cache_aligned(size_t element_size, size_t num_elements, void **orig_ptr);

#endif
