#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "atax.h"
#include "spad.h"
#include "../../common/bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"

#include "atax_kernel.h"



void __attribute__((optimize("-fno-inline")))
atax_manycore(DTYPE *a, DTYPE *_x, DTYPE *_y_partial, DTYPE *ax, int nx, int ny,
      int nx_start, int nx_end, int tid)
{
    for (int i = 0; i < ny; i++)
      _y_partial[i] = 0;
    for (int i = nx_start; i < nx_end; i++) {
      DTYPE temp=0;
      for(int j=0; j<ny; j++){
        temp += a[i*ny+j] * _x[j];
      }
      STORE_NOACK(temp, ax + i, 0);
      for(int j=0; j<ny; j++){
        _y_partial[j] += a[i*ny+j] * temp;
      }
    }
}

void __attribute__((optimize("-fno-inline"))) atax_master(int mask, DTYPE *a, DTYPE *_x, DTYPE *_y, DTYPE *ax, DTYPE *_y_partial,
      int nx, int ny, int nx_start, int nx_end, int ptid, int vtid)
{

    // DTYPE* _y_partial = malloc(ny*sizeof(DTYPE));
    #if defined _VEC
      atax_vec(mask,a,_x,_y_partial,ax,nx,ny,nx_start,nx_end,vtid);
    #else
      atax_manycore(a,_x,_y_partial,ax,nx,ny,nx_start,nx_end,ptid);
    #endif

    #ifdef _VEC
    if (!is_da) // scalar cores don't have data to accumulate so should not partcipate
    #endif
    //reduction
    
}


void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *_x, DTYPE *_y, DTYPE *ax, DTYPE *_y_partial, int nx, int ny,
    int tid_x, int tid_y, int dim_x, int dim_y)
{

  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0)
  {
    stats_on();
  }


  // linearize tid and dim
  int tid = tid_x + tid_y * dim_x;
  int dim = dim_x * dim_y;

  // split into physical and virtual tids + dim
  int ptid_x = tid_x;
  int ptid_y = tid_y;
  int ptid = tid;
  int pdim_x = dim_x;
  int pdim_y = dim_y;
  int pdim = dim;
  int vtid_x = 0;
  int vtid_y = 0;
  int vtid = 0;
  int vdim_x = 0;
  int vdim_y = 0;
  int vdim = 0;
  int start = 0;
  int end = 0;
  int orig_x = 0;
  int orig_y = 0;
  int is_da = 0;
  int master_x = 0;
  int master_y = 0;
  int unique_id = 0;
  int total_groups = 0;
  int used = 0;

  #ifdef _VEC
  #if VEC_LEN==4
  template_info_t tinfo = init_template_4x4_2x2();
  #elif VEC_LEN==16
  template_info_t tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  vtid = cinfo.vtid;
  vtid_x = cinfo.vtid_x;
  vtid_y = cinfo.vtid_y;
  vdim_x = cinfo.vdim_x;
  vdim_y = cinfo.vdim_y;
  orig_x = cinfo.orig_x;
  orig_y = cinfo.orig_y;
  is_da  = cinfo.is_scalar;
  master_x = cinfo.master_x;
  master_y = cinfo.master_y;
  unique_id = cinfo.unique_id;
  total_groups = cinfo.total_groups;
  used = cinfo.used;

  if(used){
    start = ( (unique_id + 0) * nx ) / total_groups;
    end   = ( (unique_id + 1) * nx ) / total_groups;
  }

  #else
  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  start  = ( ( ptid + 0 ) * nx ) / pdim;
  end    = ( ( ptid + 1 ) * nx ) / pdim;
  used = 1;
  #endif



  // linearize some fields
  vdim = vdim_x * vdim_y;
  int orig = orig_x + orig_y * dim_x;

  // get behavior of each core
  #ifdef _VEC
  int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
  #else
  int mask = 0;
  #endif


// region based mask for scratchpad
#ifdef _VEC
  int prefetchMask = (NUM_REGIONS << PREFETCH_NUM_REGION_SHAMT) | (REGION_SIZE << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);
#endif

  // only let certain tids continue
  if (used == 0) return;


  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later

  unsigned long long *spTop = getSpTop(ptid);
  // // guess the remaining of the part of the frame (n) that might be needed?? here n = 30
  spTop -= 30;

  unsigned long long stackLoc;
  unsigned long long temp;
  #pragma GCC unroll(30)
  for(int i=0;i<30;i++){
    asm volatile("ld t0, %[id](sp)\n\t"
                "sd t0, %[id](%[spad])\n\t"
                : "=r"(temp)
                : [id] "i"(i*8), [spad] "r"(spTop));
  }
  asm volatile (// save the stack ptr
      "addi %[dest], sp, 0\n\t"
      // overwrite stack ptr
      "addi sp, %[spad], 0\n\t"
      : [ dest ] "=r"(stackLoc)
      : [ spad ] "r"(spTop));


  atax_master(mask,a,_x,_y,ax,_y_partial,nx,ny,start,end,ptid,vtid);


  // restore stack pointer
  asm volatile(
      "addi sp, %[stackTop], 0\n\t" ::[stackTop] "r"(stackLoc));
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *_x, DTYPE *_y, DTYPE *ax, int nx, int ny,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->_x = _x;
  args->_y = _y;
  args->ax = ax;
  args->_y_partial = malloc(ny*sizeof(DTYPE));
  args->nx = nx;
  args->ny = ny;
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

  kernel(a->a, a->_x, a->_y, a->ax, a->_y_partial, a->nx, a->ny,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
