#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>
#include <sys/sysinfo.h>

#define NO_MESH 0
#define MESH_RIGHT 1
#define MESH_DOWN 2
#define MESH_LEFT 3
#define MESH_UP 4

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
  int cid = cid_x + cid_y * cores_x;
  
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early
  pthread_barrier_wait(&start_barrier);
  
  
  
  
  
  // do a specific instruction on each core
  if (cid == 0) {
    
    volatile int a;
    
    //WRITE_MESH_CSR(MESH_RIGHT);
    asm volatile (
      "csrrwi x0, 0x400, %[m0]\n\t"
      "addi %[x], x0, %[y]\n\t" 
      "csrrwi x0, 0x400, %[m1]\n\t"
      : [x] "=r" (a)
      : [y] "i" (3), [m0] "i" (MESH_RIGHT), [m1] "i" (NO_MESH)
    );
    // the second csr is causing the second mesh send, need to avoid that
    // check a flag in the instruction
    //WRITE_MESH_CSR(NO_MESH);
  }
  else if (cid == 3) {
    
    volatile int b;
    
    WRITE_MESH_CSR(MESH_UP);
    asm volatile (
      "addi %[x], x0, %[y]\n\t" 
      : [x] "=r" (b)
      : [y] "i" (2)
    );
    WRITE_MESH_CSR(NO_MESH);
  }
  else if (cid == 1) {
    //WRITE_MESH_CSR(MESH_RIGHT);
    volatile int c;
    
    // not sure what happens to variables in these regs
    // maybe save to the stack? (put the whole regfile on the stack and then return)
    
    // actually shouldn't even use the registers when in this mode
    asm volatile (
      "add %[x], x1, x2\n\t" 
      : [x] "=r" (c)
      : 
    );
    //WRITE_MESH_CSR(NO_MESH);
    
    printf("%d\n", c);
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

