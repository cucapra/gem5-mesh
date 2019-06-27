#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>
#include <sys/sysinfo.h>

#define MESH_UP 0x2

// https://forums.sifive.com/t/confusion-regarding-freedom-e-sdk-inline-asm/383
// # is stringify, 'reg' must be explictliy written out
// 'val' must be defined at compile time
// in c this means it MUST BE a define or value
// in c++ it can be define, value, or const int
#define WRITE_CSR(reg, val) \
  asm volatile ("csrrwi x0, " #reg ", %[x]\n\t" :: [x] "i" (val))

// 0x400 is the csr specified in gem5 in src/arch/riscv/register.hh
#define WRITE_MESH_CSR(val) \
  WRITE_CSR(0x400, val)

pthread_barrier_t start_barrier;

typedef struct kern_args_t {
  int cid_x, cid_y;
  int cores_x, cores_y;
} kern_args_t;


void *kernel(void* args) {
  // unpack args
  kern_args_t *ka = (kern_args_t*)args;
  int cid_x = ka->cid_x;
  int cid_y = ka->cid_y;
  int cores_x = ka->cores_x;
  int cores_y = ka->cores_y;
  
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early
  pthread_barrier_wait(&start_barrier);
  
  
  
  WRITE_MESH_CSR(MESH_UP);
  
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

