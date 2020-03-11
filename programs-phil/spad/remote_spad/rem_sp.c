#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "rem_sp.h"
#include "spad.h"
#include "../../common/bind_defs.h"



#define REGION_SIZE 8
#define NUM_REGIONS (512/REGION_SIZE)

//#define REMOTE


void __attribute__((optimize("-fno-inline")))
rem_sp(DTYPE* a, DTYPE* c, int start, int end, int tid, int total_cores) {


  DTYPE* sp_adj = (DTYPE*)getSpAddr((tid+1)%total_cores,0);
  DTYPE* sp_own = (DTYPE*)getSpAddr(tid,0);
  int spadRegion = 0;

  
  #ifdef REMOTE
  DTYPE* read_region = sp_adj;
  #else 
  DTYPE* read_region = sp_own;
  #endif

  read_region += REGION_SIZE*NUM_REGIONS;
  volatile DTYPE* write_region = sp_own + REGION_SIZE*NUM_REGIONS + REGION_SIZE; //volatile to avoid compiler optimizing it away
  volatile DTYPE write_var=0;

  for (int i = start; i < end; i+=REGION_SIZE){

    //read first into adj scratch pad
    // for(int j=0; j<REGION_SIZE; j++){
    //   read_region[j]= a[i+j];
    // }
   
    //write to own scratch pad with some modifications
    for(int j=0; j<REGION_SIZE; j++){
      // write_region[j] = read_region[j]*5;
      write_var += read_region[j]*5;
    }

    // //write to DRAM
    // for(int j=0; j<REGION_SIZE; j++){
    //   c[i+j] = write_region[j];
    // }

  }
  

}

// actual kernel
void kernel(
    DTYPE *a,DTYPE *c, int n,
    int tid_x, int tid_y, int dim_x, int dim_y) {


  
  int total_cores = dim_x*dim_y;
  int tid = tid_x + tid_y * dim_x;
  int orig_x = 0;
  int orig_y = 0; //only have 1 group for now

  int start = tid * (n/total_cores) ;
  int end = start + (n/total_cores) ;



  // start recording all stats (all cores)
  // use the last thread, b/c this wakes up last?
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }
  

  int prefetchMask = (NUM_REGIONS << PREFETCH_NUM_REGION_SHAMT) | (REGION_SIZE << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);
  pthread_barrier_wait(&start_barrier);

 
  rem_sp(a, c,  start, end, tid, total_cores);

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *c, int n,
  int tid_x, int tid_y, int dim_x, int dim_y) {
      
  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->c = c;
  args->n = n;
  args->tid_x = tid_x;
  args->tid_y = tid_y;
  args->dim_x = dim_x;
  args->dim_y = dim_y;
  
  return args;
      
}

void *pthread_kernel(void *args) {
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early
  
  pthread_barrier_wait(&start_barrier);
  
  // call the spmd kernel
  Kern_Args *a = (Kern_Args*)args;

  kernel(a->a,a->c, a->n,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  if (a->tid_x == 1 && a->tid_y == 1) {
    stats_off();
  }

     
  return NULL;
}
