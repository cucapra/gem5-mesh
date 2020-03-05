
#include "pthread_launch.h"
#include "spad.h"
#include <stdlib.h>
#include <math.h>
#include <sys/sysinfo.h>
#include <stdio.h>

#define PTHREAD_STACK_ON_SPAD 1
 
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
    // default thread attributes
    pthread_attr_t attr;
    pthread_attr_init(&attr);

    // if you would like the pthread stack to go on the spad
    // then need to specify it here. Can't do it in gem5 because
    // the pthread routine will copy data to the new stack and will
    // give the stack ptr as an argument to the clone syscall
    // http://man7.org/linux/man-pages/man3/pthread_attr_setstack.3.html
    #ifdef PTHREAD_STACK_ON_SPAD
    void *attrStackAddr0;
    size_t size0;
    pthread_attr_getstack(&attr, &attrStackAddr0, &size0);

    if (pthread_attr_setstack(&attr, getSpTop(i + 1), getSpadNumBytes()) != 0) {
      //  EINVAL stacksize is less than PTHREAD_STACK_MIN (16384) bytes.  On
      // some systems, this error may also occur if stackaddr or
      // stackaddr + stacksize is not suitably aligned.
      printf("fail to set stack size, prob not enough space @ %d!\n", getSpadNumBytes());
    }
    pthread_attr_setstackaddr(&attr, getSpTop(i + 1));
    // pthread_attr_setstacksize(&attr, getSpadNumBytes());
    void *attrStackAddr1;
    size_t size1;
    pthread_attr_getstack(&attr, &attrStackAddr1, &size1);
    printf("core %d stck addr %p %llu -> %p %llu\n", i + 1, attrStackAddr0, size0, attrStackAddr1, size1);
    #endif
    pthread_create(threads[i], &attr, kernel, args[i + 1]);

    // can free attr memory as soon as create
    pthread_attr_destroy(&attr);
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
  //
  // this line does not work with iocpu, gets stuck on a trap
  // three options
  // 1) fix iocpu
  // 2) set and then read a csr with this info
  // 3) since need to specify number of sps anyway, just use that for core count
  //int total_cpus = get_nprocs();
  int total_cpus = _N_SPS;
  return total_cpus;
}

int get_dimensions(int *cores_x, int *cores_y) {
  int total_cpus = get_cores();

  // figure out device dimensions
  int width = (int)sqrt(total_cpus);
  int height = (float)total_cpus / (float)width;
  *cores_x = width;
  *cores_y = height;
  return width * height;
}


void *malloc_cache_aligned(size_t element_size, size_t num_elements, void **orig_ptr) {
  void *ptr = malloc(element_size * (num_elements + 64));
  *orig_ptr = ptr;
  if (element_size == sizeof(int)) {
    ptr = (void*)((unsigned long long)((int*)ptr + 64) & ~((1ULL << 6) - 1));
  }
  else {
    return NULL;
  }
  return ptr;
}