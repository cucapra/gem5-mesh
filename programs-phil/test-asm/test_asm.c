#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>
#include <assert.h>
#include <sys/sysinfo.h>

#include "common/bind_defs.h"

pthread_barrier_t start_barrier;

typedef struct kern_args_t {
  int cid_x, cid_y;
  int cores_x, cores_y;
} kern_args_t;

// declare some global mem to use in program
int mem[10] = { 4, 5, 40, 50, 40, 50, 40, 50, 40, 50 };

void *kernel(void* args) {
  // unpack args
  kern_args_t *ka = (kern_args_t*)args;
  int cid_x = ka->cid_x;
  int cid_y = ka->cid_y;
  int cores_x = ka->cores_x;
  int cores_y = ka->cores_y;
  int cid = cid_x + cid_y * cores_x;
  
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early
  pthread_barrier_wait(&start_barrier);
  
  // do a specific instruction on each core
  if (cid == 0) {
    
    int a0, a1, a2;
    
    int *virtualMemAddr = &(mem[0]);
    
    BINDED_SECTION(O_RIGHT_RD, ALL_NORM, 
      "ld   %[a1], 0(%[m0])\n\t"
      "addi %[a0], x0, %[i0]\n\t"
      "addi %[a2], x0, %[i1]\n\t" 
      ,
      [a0] "=r" (a0) COMMA [a1] "=r" (a1) COMMA [a2] "=r" (a2)
      ,
      COMMA [i0] "i" (2) COMMA [i1] "i" (27) 
      COMMA [m0] "r" (virtualMemAddr)
      
      );
    
    
  }
  else if (cid == cores_x + 1) {
    
    int b0, b1, b2;
    
    int *virtualMemAddr = &(mem[1]);
    
    BINDED_SECTION(O_UP_RD, ALL_NORM, 
      "addi %[b0], x0, %[i0]\n\t"
      "addi %[b1], x0, %[i1]\n\t"
      "ld   %[b2], 0(%[m0])\n\t"
      ,
      [b0] "=r" (b0) COMMA [b1] "=r" (b1) COMMA [b2] "=r" (b2)
      ,
      COMMA [i0] "i" (10) COMMA [i1] "i" (7)
      COMMA [m0] "r" (virtualMemAddr)
      
      );
      
    
  }
  else if (cid == 1) {
    
    //int *virtualMemAddr = &(mem[2]);
    
    int c0, c1, c2;
    int a = 2;
    int b = 2;
    BINDED_SECTION(I_RS1_DOWN | I_RS2_LEFT, ALL_NORM, 
      "add %[c0], %[a], %[b]\n\t"
      "add %[c1], %[a], %[b]\n\t"
      "add %[c2], %[a], %[b]\n\t"
      , 
      [c0] "=r" (c0) COMMA [c1] "=r" (c1) COMMA [c2] "=r" (c2)
      , 
      COMMA [a] "r" (a) COMMA [b] "r" (b)
      );
      
    
    printf("%d %d %d\n", c0, c1, c2);
    assert(c0 == 14 && c1 == 9 && c2 == 32);
  }
  
}

int main(int argc, char* argv[])
{
  // get the number of processors in the system
  // would rather use multi-platform thread::hardware_concurreny
  // but doesn't work in c (only c++11)
  int total_cpus = get_nprocs();
  printf("Hello world! %d\n", total_cpus);

  // TODO this work is redundant with both python config and other c progs
  // use 1 cpu to do setup and monitoring (like a host)
  int device_cpus = total_cpus - 1;

  // figure out device dimensions
  int width = (int)sqrt(device_cpus);
  int height = (float)device_cpus / (float)width;
  int cores_x = width;
  int cores_y = height;
  int num_cores = cores_x * cores_y;
  
  // device threads will be pthreads
  pthread_t **threads = (pthread_t**)malloc(sizeof(pthread_t*) * num_cores);
  
  // barrier to guarentee one thread per core (prevents from any finishing
  // before scheduling)
  pthread_barrier_init(&start_barrier, NULL, num_cores);

  // initialize the arguments to send to each device core
  kern_args_t **kern_args = (kern_args_t**)malloc(sizeof(kern_args_t*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      threads[i] = (pthread_t*)malloc(sizeof(pthread_t));
   
      kern_args_t *ka = (kern_args_t*)malloc(sizeof(kern_args_t));
      ka->cid_x = x;
      ka->cid_y = y;
      ka->cores_x = cores_x;
      ka->cores_y = cores_y;
      kern_args[i] = ka;
    }  
  }

  // create a thread on each device core
  for (int i = 0; i < num_cores; i++) {
    void *args = (void*)(kern_args[i]);
		pthread_create(threads[i], NULL, kernel, args);
  }

  // wait for threads to finish
  for (int i = 0; i < num_cores; i++) {   
    pthread_join(*(threads[i]), NULL);
  }
  
  // cleanup
  for (int i = 0; i < num_cores; i++) {
    free(kern_args[i]);
    free(threads[i]);
  }
  free(kern_args);
  free(threads);

  return 0;
}

