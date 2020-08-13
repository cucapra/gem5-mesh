#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "corr.h"
#include "spad.h"
#include "bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"
#include "reduction.h"
#include "util.h"

#include "corr_kernel.h"

static inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

#ifdef OPTIMIZED_TRANSPOSE
void transpose_manycore(DTYPE *a, int a_row, int a_col, DTYPE *aT, int ptid, int pdim){

  int start = (ptid + 0) * a_col / pdim;
  int end = (ptid + 1) * a_col / pdim;

  for(int i=start; i<end; i++){
    for(int j=0; j<a_row; j++){
      aT[i*a_row+j] = a[j*a_col+i];
    }
  }

}


void __attribute__((optimize("-fno-inline")))
corr_manycore_1(DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int end, int ptid, float eps)
{
  // double eps = 0.1f;

  DTYPE mean_temp=0;
  DTYPE stddev_temp=0;
  DTYPE data_temp;

  #ifdef MANYCORE_PREFETCH
  int spadRegion = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);
  int sp_offset;
  #endif

  for (int i = start; i < end; i++){
    //mean
    mean_temp = 0;

    #ifdef MANYCORE_PREFETCH
    PF_BEGIN(REGION_SIZE)
    PF1(sp_offset,_idx_(i,j,n))
    {
      mean_temp += spAddr[sp_offset+jj];
    }
    PF_END(NUM_REGIONS)
    #else
    for (int j = 0; j < n; j++)
      mean_temp += data[i*n+j];
    #endif
    mean_temp /= n;
    // mean[i]=mean_temp;
    STORE_NOACK(mean_temp,mean+i,0);

    //stddev
    stddev_temp = 0;

    #ifdef MANYCORE_PREFETCH
    PF_BEGIN(REGION_SIZE)
    PF1(sp_offset,_idx_(i,j,n))
    {
      stddev_temp += (spAddr[sp_offset+jj]-mean_temp)*(spAddr[sp_offset+jj]-mean_temp);
    }
    PF_END(NUM_REGIONS)
    #else
    for (int j = 0; j < n; j++)
      stddev_temp += (data[i*n+j]-mean_temp)*(data[i*n+j]-mean_temp);
    #endif
    stddev_temp = stddev_temp/n;
    stddev_temp = sqrt(stddev_temp);
    stddev_temp = stddev_temp <= eps ? 1.0 : stddev_temp;
    // stddev[i] = stddev_temp;
    STORE_NOACK(stddev_temp,stddev+i,0);

    //center

    #ifdef MANYCORE_PREFETCH
    PF_BEGIN(REGION_SIZE)
    PF1(sp_offset,_idx_(i,j,n))
    {
      data_temp = spAddr[sp_offset+jj]-mean_temp;
      data_temp = data_temp/(sqrt(n)*stddev_temp);
      // data[i*n+(j+jj)] = data_temp;
      STORE_NOACK(data_temp,data+(i*n)+(j+jj),0);
    }
    PF_END(NUM_REGIONS)
    #else
    for (int j = 0; j < n; j++){
      data_temp = data[i*n+j]-mean_temp;
      data[i*n+j] = data_temp/(sqrt(n)*stddev_temp);
    }
    #endif

    symmat[i*m+i]=1; //make diagonal 1 for the vectors it is assigned
    asm volatile("fence\n\t");
  }
    
}

void __attribute__((optimize("-fno-inline")))
corr_manycore_2(DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int stride, int ptid)
{

  #ifdef MANYCORE_PREFETCH
  int spadRegion = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);
  int sp_i1_offset,sp_i2_offset;
  #endif

  DTYPE sym_temp=0;
  for (int i1 = start; i1 < m-1; i1+=stride){
    for (int i2 = i1+1; i2 < m; i2++){
      sym_temp=0;
      #ifdef MANYCORE_PREFETCH
      PF_BEGIN(REGION_SIZE_K2/2)
      PF2(sp_i1_offset,sp_i2_offset,_idx_(i1, j, n),_idx_(i2, j, n))
      {
        sym_temp+=spAddr[sp_i1_offset+jj]*spAddr[sp_i2_offset+jj];
      }
      PF_END(NUM_REGIONS_K2)
      #else
      for(int j=0; j<n; j++){
        sym_temp+=data[i1*n+j]*data[i2*n+j];
      }
      #endif
      STORE_NOACK(sym_temp,symmat+(i1*m)+i2,0);
      STORE_NOACK(sym_temp,symmat+(i2*m)+i1,0);
      // symmat[i1*m+i2]=sym_temp;
      // symmat[i2*m+i1]=sym_temp;
    }
  }

  asm volatile("fence\n\t");
}

#elif defined POLYBENCH_VERSION
void __attribute__((optimize("-fno-inline")))
corr_manycore_1(DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int end, int ptid, float eps)
{
  // double eps = 0.1f;

  DTYPE mean_temp=0;
  DTYPE stddev_temp=0;
  DTYPE data_temp;

  #ifdef MANYCORE_PREFETCH
  int spadRegion = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);
  int sp_offset;
  #endif

  for (int j = start; j < end; j++){
    //mean
    mean_temp = 0;
    #ifdef MANYCORE_PREFETCH
    PF_BEGIN(REGION_SIZE)
    PF1(sp_offset,i,j,m)
    {
      mean_temp += spAddr[sp_offset+jj];
    }
    PF_END(NUM_REGIONS)
    #else
    for (int i = 0; i < n; i++)
      mean_temp += data[i*m+j];
    #endif
    mean_temp /= n;
    // mean[j]=mean_temp;
    STORE_NOACK(mean_temp,mean+j,0);

    //stddev
    stddev_temp = 0;

    #ifdef MANYCORE_PREFETCH
    PF_BEGIN(REGION_SIZE)
    PF1(sp_offset,i,j,m)
    {
      stddev_temp += (spAddr[sp_offset+jj]-mean_temp)*(spAddr[sp_offset+jj]-mean_temp);
    }
    PF_END(NUM_REGIONS)
    #else
    for (int i = 0; i < n; i++)
      stddev_temp += (data[i*m+j]-mean_temp)*(data[i*m+j]-mean_temp);
    #endif
    stddev_temp = stddev_temp/n;
    stddev_temp = sqrt(stddev_temp);
    stddev_temp = stddev_temp <= eps ? 1.0 : stddev_temp;
    // stddev[j] = stddev_temp;
    STORE_NOACK(stddev_temp,stddev+j,0);

    //center
    #ifdef MANYCORE_PREFETCH
    PF_BEGIN(REGION_SIZE)
    PF1(sp_offset,i,j,m)
    {
      data_temp = spAddr[sp_offset+jj]-mean_temp;
      data_temp = data_temp/(sqrt(n)*stddev_temp);
      // data[(i+jj)*m+(j)] = data_temp;
      STORE_NOACK(data_temp,data+((i+jj)*m)+j,0);
    }
    PF_END(NUM_REGIONS)
    #else
    for (int i = 0; i < n; i++){
      data_temp = data[i*m+j]-mean_temp;
      data[i*m+j] = data_temp/(sqrt(n)*stddev_temp);
    }
    #endif

    symmat[j*m+j]=1; //make diagonal 1 for the vectors it is assigned
    asm volatile("fence\n\t");
  }
    
}

void __attribute__((optimize("-fno-inline")))
corr_manycore_2(DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int stride, int ptid)
{

  #ifdef MANYCORE_PREFETCH
  int spadRegion = 0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);
  int sp_i1_offset,sp_i2_offset;
  #endif

  DTYPE sym_temp=0;
  for (int j1 = start; j1 < m-1; j1+=stride){
    for (int j2 = j1+1; j2 < m; j2++){
      sym_temp=0;
      #ifdef MANYCORE_PREFETCH
      PF_BEGIN(REGION_SIZE_K2/2)
      PF2(sp_i1_offset,sp_i2_offset,i, j1, j2, m)
      {
        sym_temp+=spAddr[sp_i1_offset+jj]*spAddr[sp_i2_offset+jj];
      }
      PF_END(NUM_REGIONS_K2)
      #else
      for(int i=0; i<n; i++){
        sym_temp+=data[i*m+j1]*data[i*m+j2];
      }
      #endif
      STORE_NOACK(sym_temp,symmat+(j1*m)+j2,0);
      STORE_NOACK(sym_temp,symmat+(j2*m)+j1,0);
      // symmat[j1*m+j2]=sym_temp;
      // symmat[j2*m+j1]=sym_temp;
    }
  }
  asm volatile("fence\n\t");
}
#endif



void kernel(DTYPE *data, DTYPE *dataT, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y)
{

  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0)
  {
    stats_on();
  }

  

  // linearize tid and dim
  int ptid = ptid_x + ptid_y * pdim_x;
  int pdim = pdim_x * pdim_y;
  int start = 0;
  int end = 0;
  float eps=0.1;
  template_info_t tinfo;

  //transpose matrix
  #ifdef OPTIMIZED_TRANSPOSE
  transpose_manycore(data,n,m,dataT,ptid,pdim);
  data=dataT;
  #endif

  #ifdef _VEC
  #if VEC_LEN==4
  tinfo = init_template_4x4_2x2();
  #elif VEC_LEN==16
  tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  
  if(cinfo.used){
    //do work division here
    int alignment = VEC_LEN; //each group should have elements of multiple of this number
    start = roundUp((cinfo.unique_id + 0) * m / cinfo.total_groups, alignment); 
    end = roundUp((cinfo.unique_id + 1) * m / cinfo.total_groups, alignment); 
  }

  #else
  core_config_info_t cinfo = manycore_template(ptid_x, ptid_y, pdim_x, pdim_y);
  
  //do work division here
  start  = ( ( ptid + 0 ) * m ) / pdim;
  end    = ( ( ptid + 1 ) * m ) / pdim;
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

  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later

  unsigned long long *spTop = getSpTop(ptid);
  // // guess the remaining of the part of the frame (n) that might be needed?? here n = 30
  spTop -= 50;

  unsigned long long stackLoc;
  unsigned long long temp;
  #pragma GCC unroll(50)
  for(int i=0;i<50;i++){
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

  if(cinfo.used!=0){
    #if defined _VEC
      tril_corr_vec_1(mask, data, symmat, mean, stddev, m, n, start, end, cinfo.vtid, cinfo.vdim_x*cinfo.vdim_y, ptid, eps);
    #else
      VECTOR_EPOCH(mask);
      corr_manycore_1(data, symmat, mean, stddev, m, n, start, end, ptid, eps);
    #endif
  }

  #ifdef _VEC
  SET_PREFETCH_MASK(NUM_REGIONS_K2, REGION_SIZE_K2, &start_barrier);
  #elif defined MANYCORE_PREFETCH
  SET_PREFETCH_MASK(NUM_REGIONS_K2,REGION_SIZE_K2,&start_barrier);
  #else
  pthread_barrier_wait(&start_barrier);
  #endif

  // if (ptid== 0)
  // {
  //   stats_on();
  // }

  if (cinfo.used == 0) goto stack_end;
  //redistribute work for 2nd kernel
  #ifdef _VEC
  start = cinfo.unique_id*VEC_LEN;
  int stride = cinfo.total_groups*VEC_LEN;
  #else
  start  = ptid;
  int stride = pdim;
  #endif

  #if defined _VEC
    tril_corr_vec_2(mask,data, symmat, mean, stddev, m, n, start, stride, cinfo.vtid, cinfo.vdim_x*cinfo.vdim_y, ptid);
  #else
    VECTOR_EPOCH(mask);
    corr_manycore_2(data, symmat, mean, stddev, m, n, start, stride, ptid);
  #endif

  stack_end:
  // restore stack pointer
  asm volatile(
      "addi sp, %[stackTop], 0\n\t" ::[stackTop] "r"(stackLoc));
}

// helper functions
Kern_Args *construct_args(DTYPE *data, DTYPE *dataT, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->data = data;
  args->dataT = dataT;
  args->symmat = symmat;
  args->mean = mean; 
  args->stddev = stddev;
  args->m = m;
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

  kernel(a->data, a->dataT, a->symmat, a->mean, a->stddev, a->m, a->n,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
