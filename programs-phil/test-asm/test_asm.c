#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>
#include <sys/sysinfo.h>

// 20 bit / 5 hex
#define ALL_NORM  0x00000

#define BITS_RD   3
#define RD_NORM   0x0
#define RD_RIGHT  0x1
#define RD_DOWN   0x2
#define RD_LEFT   0x3
#define RD_UP     0x4

#define BITS_RS1  3
#define SHAMT_RS1 BITS_RD
#define RS1_NORM  0x0 << SHAMT_RS1
#define RS1_RIGHT 0x1 << SHAMT_RS1
#define RS1_DOWN  0x2 << SHAMT_RS1
#define RS1_LEFT  0x3 << SHAMT_RS1
#define RS1_UP    0x4 << SHAMT_RS1

#define BITS_RS2  3
#define SHAMT_RS2 SHAMT_RS1 + BITS_RS1
#define RS2_NORM  0x0 << SHAMT_RS2
#define RS2_RIGHT 0x1 << SHAMT_RS2
#define RS2_DOWN  0x2 << SHAMT_RS2
#define RS2_LEFT  0x3 << SHAMT_RS2
#define RS2_UP    0x4 << SHAMT_RS2

#define COMMA ,

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

#define BIND_EXE(val) \
  asm volatile (".insn u 0x6b, x0, %[x]\n\t" :: [x] "i" (val))

// to ensure that the compiler doesn't place unwanted instructions
// within the binds we enforce with a single asm volatile
#define BINDED_SECTION(sbind, ebind, code, wr, rd)  \
  asm volatile (                                    \
    ".insn u 0x6b, x0, %[b0]\n\t"                 \
    code                                            \
    ".insn u 0x6b, x0, %[b1]\n\t"                 \
    : wr                                            \
    : [b0] "i" (sbind), [b1] "i" (ebind) rd        \
  )


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
    
    int a;
    
    /*BIND_EXE(RD_RIGHT);
    
    asm volatile (
      "addi %[x], x0, %[y]\n\t"
      : [x] "=r" (a)
      : [y] "i" (2)
    );
    
    BIND_EXE(RD_NORM);*/
    
    BINDED_SECTION(RD_RIGHT, ALL_NORM, 
      "addi %[x], x0, %[y]\n\t", 
      [x] "=r" (a), 
      COMMA [y] "i" (2));
    
    
  }
  else if (cid == 3) {
    
    int b;
    
    /*//WRITE_MESH_CSR(MESH_UP);
    BIND_EXE(RD_UP);
    asm volatile (
      "addi %[x], x0, %[y]\n\t" 
      : [x] "=r" (b)
      : [y] "i" (2)
    );
    //WRITE_MESH_CSR(NO_MESH);
    BIND_EXE(RD_NORM);*/
    
    BINDED_SECTION(RD_UP, ALL_NORM, 
      "addi %[x], x0, %[y]\n\t", 
      [x] "=r" (b), 
      COMMA [y] "i" (10));
    
  }
  else if (cid == 1) {
    /*BIND_EXE(RS1_UP | RS2_RIGHT);
    volatile int c;
    asm volatile (
      "add %[x], x1, x2\n\t" 
      : [x] "=r" (c)
      : 
    );
    
    // an instrcution is being put between here!
    BIND_EXE(ALL_NORM);*/
    
    int c;
    int a = 2;
    int b = 2;
    BINDED_SECTION(RS1_DOWN | RS2_LEFT, ALL_NORM, 
      "add %[x], %[y], %[z]\n\t", 
      [x] "=r" (c), 
      COMMA [y] "r" (a) COMMA [z] "r" (b));
      
    
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

