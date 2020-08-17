#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "mvt.h"
#include "spad.h"
#include "bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"
#include "reduction.h"
#include "util.h"

#include "mvt_kernel.h"

#define STACK_COPY


void __attribute__((optimize("-fno-inline")))
mvt_manycore(DTYPE *a, DTYPE *y1, DTYPE *y2, DTYPE *x1, DTYPE *x2, int n, int start, int end, int ptid)
{
  DTYPE temp;

  #ifdef MANYCORE_PREFETCH
  int spadRegion = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);
  int sp_a_offset, sp_y_offset;
  #endif

  for (int i = start; i < end; i++) {
      temp=0;
      #ifdef MANYCORE_PREFETCH
      PF_BEGIN(REGION_SIZE/2)
      PF2(sp_a_offset,sp_y_offset,a,y1,i*n+j,j)
      {
        temp+= spAddr[sp_a_offset+jj]*spAddr[sp_y_offset+jj];
      }
      PF_END(NUM_REGIONS)
      #else
      #pragma GCC unroll(16)
      for(int j=0; j<n; j++){
        temp += a[i*n+j] * y1[j];
      }
      #endif
      x1[i]+=temp;
      temp=0;
      #ifdef MANYCORE_PREFETCH
      PF_BEGIN(REGION_SIZE)
      PF1(sp_y_offset,y2,j)
      {
        temp+=a[(j+jj)*n+i] * spAddr[sp_y_offset+jj];
      }
      PF_END(NUM_REGIONS)
      #else
      #pragma GCC unroll(16)
      for(int j=0; j<n; j++){
        temp+= a[j*n+i] * y2[j];
      }
      #endif
      x2[i]+=temp;
  }
}

void __attribute__((optimize("-fno-inline")))
mvt_main(core_config_info_t cinfo, int mask, DTYPE *a, DTYPE *y1, DTYPE *y2, DTYPE *x1, DTYPE *x2, int n, 
                  int start, int end, int ptid, int pdim, int pdim_x, template_info_t tinfo){

  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later
  
  #ifdef STACK_COPY
  MOVE_STACK_ONTO_SCRATCHPAD();
  #endif

  if(cinfo.used!=0){
    // if(ptid==0)printf("2. ptid pointer %x\n",ptid_group);
    #if defined _VEC
      tril_mvt_vec(mask,a,y1,y2,x1,x2,n,start,end,ptid, cinfo.vtid);

    #else
      VECTOR_EPOCH(mask);
      mvt_manycore(a,y1,y2,x1,x2,n,start,end,ptid);
    #endif
  }

  #ifdef STACK_COPY
  RECOVER_DRAM_STACK();
  #endif
}

void kernel(DTYPE *a, DTYPE *y1, DTYPE *y2, DTYPE *x1, DTYPE *x2, int n,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y)
{

  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0)
  {
    stats_on();
  }


  
  int ptid = ptid_x + ptid_y * pdim_x;
  int pdim = pdim_x * pdim_y;
  int start = 0;
  int end = 0;
  template_info_t tinfo;
  // int* ptid_group = getSpAddr(ptid,REGION_SIZE*NUM_REGIONS);

  #ifdef _VEC
  #if VEC_LEN==4
  tinfo = init_template_4x4_2x2();
  #elif VEC_LEN==16
  tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  // if(ptid==0) printf("Total groups %d\n",cinfo.total_groups);
  if(cinfo.used){
    //do work division here
    int alignment = VEC_LEN; //each group should have elements of multiple of this number
    start = roundUp((cinfo.unique_id + 0) * n / cinfo.total_groups, alignment); 
    end = roundUp((cinfo.unique_id + 1) * n / cinfo.total_groups, alignment); 

    // if(cinfo.is_scalar==1) printf("ptid:%d, start=%d and end=%d\n",ptid,start,end);
  }

  #else
  core_config_info_t cinfo = manycore_template(ptid_x, ptid_y, pdim_x, pdim_y);
  
  //do work division here
  start  = ( ( ptid + 0 ) * n ) / pdim;
  end    = ( ( ptid + 1 ) * n ) / pdim;
  #endif

  // get behavior of each core
  #ifdef _VEC
  int mask = getSIMDMask(&cinfo);
  #elif defined MANYCORE_PREFETCH
  int mask = getDebugMask(&cinfo);
  #else
  int mask = 0;
  #endif


  // region based mask for scratchpad
#ifdef _VEC
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
#elif defined MANYCORE_PREFETCH
  SET_PREFETCH_MASK(NUM_REGIONS,REGION_SIZE,&start_barrier);
#endif

// only let certain tids continue
  // if (used == 0) return;

  mvt_main(cinfo, mask,a,y1,y2,x1,x2,n,start,end,ptid, pdim, pdim_x, tinfo);
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *y1, DTYPE *y2, DTYPE *x1, DTYPE *x2, int n,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->y1 = y1;
  args->y2 = y2;
  args->x1 = x1;
  args->x2 = x2;
  args->n = n;
  args->tid_x = tid_x;
  args->tid_y = tid_y;
  args->dim_x = dim_x;
  args->dim_y = dim_y;

  return args;
}

void *pthread_kernel(void *args)
{
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early

  pthread_barrier_wait(&start_barrier);

  // call the spmd kernel
  Kern_Args *a = (Kern_Args *)args;

  kernel(a->a, a->y1, a->y2, a->x1, a->x2, a->n,
          a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  pthread_barrier_wait(&start_barrier);
  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
