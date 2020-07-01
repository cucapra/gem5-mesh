#include "corr_kernel.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

inline void prefetch_data_frame (DTYPE* data, int i, int j, int n, int vdim, int *sp_data_offset){
  for (int d = 0; d < vdim; d++){
    VPREFETCH_L(*sp_data_offset, data + _idx_(i+d,j,n), d, REGION_SIZE,1); //vertical loads
  }
  *sp_data_offset = *sp_data_offset + REGION_SIZE;
  if(*sp_data_offset==NUM_REGIONS*REGION_SIZE)*sp_data_offset=0;
}


void tril_corr_vec_1(int mask, DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int end, int vtid, int vdim, int ptid, float eps)
{
  //this template uses separate scalar and vector code blocks but they can be interspersed as well as shown here
  //https://github.com/cucapra/gem5-mesh/wiki/Trilliasm-Language-Overview:-Vector-SIMD-in-C

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  //---------------------------------
  //scalar core code iterspersed with vissue
  ISSUE_VINST(init_label);
  //prefetch variables
  int spadRegion = 0;
  int sp_data_offset=0;

  for (int i = start; i < end; i+=vdim){
    ISSUE_VINST(hoist1_label);
    //mean
    for (int j = 0; j < n; j+=REGION_SIZE){
      prefetch_data_frame(data,i,j,n,vdim,&sp_data_offset);
      ISSUE_VINST(mean_label);
    }

    ISSUE_VINST(hoist2_label);
    //stdev
    for (int j = 0; j < n; j+=REGION_SIZE){
      prefetch_data_frame(data,i,j,n,vdim,&sp_data_offset);
      ISSUE_VINST(stddev_label);
    }

    ISSUE_VINST(hoist3_label);
    //center
    for (int j = 0; j < n; j+=REGION_SIZE){
      prefetch_data_frame(data,i,j,n,vdim,&sp_data_offset);
      ISSUE_VINST(center_label);
    }

    ISSUE_VINST(symmat1_label);
  }


  //issue stack end portions of vector cores
  ISSUE_VINST(vector_stack_label);
  // devec with unique tag
  DEVEC(devec_0);

  //fence for all cores to ensure memory operations have completed
  asm volatile("fence\n\t");

  asm("trillium vissue_delim return scalar_return"); //return delimiter, delimiters can be of many types
  return;

  //all the vissue labels below:

  init_label: //this name matches with vissue label name
    asm("trillium glue_point init"); //name over here "init" matches with delimiter in vector code
  hoist1_label:
    asm("trillium glue_point hoist1");
  mean_label:
    asm("trillium glue_point mean");
  hoist2_label:
    asm("trillium glue_point hoist2");
  stddev_label:
    asm("trillium glue_point stddev");
  hoist3_label:
    asm("trillium glue_point hoist3");
  center_label:
    asm("trillium glue_point center");
  symmat1_label:
    asm("trillium glue_point symmat1");
  vector_stack_label: 
    asm("trillium glue_point vector_stack"); //name over here "vector_stack" matches with delimiter in vector code

  #elif defined VECTOR_CORE
  asm("trillium vissue_delim until_next init"); //until_next delimiter used, name (init) over here same as in glue point above
  //vector core code
  volatile int bh1,bh2,bh3;
  
  int sp_offset=0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);

  int i=start+vtid;
  int j=0;
  DTYPE mean_temp=0;
  DTYPE stddev_temp=0;
  DTYPE data_temp;
  // double eps = 0.1;

  do{
    asm("trillium vissue_delim until_next hoist1");
    mean_temp=0;
    do{
      asm("trillium vissue_delim until_next mean");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(8)
      for(int jj=0; jj<REGION_SIZE; jj++){
        mean_temp+= spAddr[sp_offset+jj];
      }
      REMEM(REGION_SIZE);
      sp_offset += REGION_SIZE;
      // if(sp_offset==NUM_REGIONS)sp_offset=0;
      sp_offset = sp_offset%(NUM_REGIONS*REGION_SIZE);
    } while(bh2); 
    asm("trillium vissue_delim until_next hoist2");
    mean_temp/=n;
    mean[i]=mean_temp;

    stddev_temp=0;
    do {
      asm("trillium vissue_delim until_next stddev");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(8)
      for(int jj=0; jj<REGION_SIZE; jj++){
        stddev_temp+= (spAddr[sp_offset+jj]-mean_temp)*(spAddr[sp_offset+jj]-mean_temp);
      }
      REMEM(REGION_SIZE);
      sp_offset += REGION_SIZE;
      // if(sp_offset==NUM_REGIONS)sp_offset=0;
      sp_offset = sp_offset%(NUM_REGIONS*REGION_SIZE);
    } while(bh2);
    asm("trillium vissue_delim until_next hoist3");
    stddev_temp = stddev_temp/n;
    stddev_temp = sqrt(stddev_temp);
    
    int cond = stddev_temp <= eps;
    volatile int compiler_hack = 1;
    PRED_EQ(cond,1);
    if(compiler_hack){
      stddev_temp = 1.0;
    }
    PRED_EQ(ptid,ptid);
    stddev[i] = stddev_temp;

    j=0;
    do {
      asm("trillium vissue_delim until_next center");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(8)
      for(int jj=0; jj<REGION_SIZE; jj++){
        data_temp= (spAddr[sp_offset+jj]-mean_temp);
        data_temp/=(sqrt(n)*stddev_temp);
        data[i*n+j+jj]= data_temp;
      }
      REMEM(REGION_SIZE);
      sp_offset += REGION_SIZE;
      // if(sp_offset==NUM_REGIONS)sp_offset=0;
      sp_offset = sp_offset%(NUM_REGIONS*REGION_SIZE);
      j+=REGION_SIZE;
    } while(bh2);
    asm("trillium vissue_delim until_next symmat1");
    symmat[i*m+i]=1; //make diagonal 1 for the vectors it is assigned
    i+=vdim;
  } while(bh1);

  asm("trillium vissue_delim return vector_stack"); //return delimiter
  return;
  #endif

}


void tril_corr_vec_2(int mask, DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int stride, int vtid, int vdim, int ptid){


  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);
  if(ptid==0){
    printf("Hello\n");
  }

  //---------------------------------
  //scalar core code iterspersed with vissue
  ISSUE_VINST(init_label); //eg: this block will deal with initila stack manipulation and initilaization of variables
  //-----------------------------------
  //prefetch variables
  int spadRegion = 0;
  int sp_data_offset=0;

  if(ptid==0){
    printf("Hello again\n");
  }
  for (int i1 = start; i1 < m-1; i1+=stride){
    ISSUE_VINST(hoist1_label);
    for(int i2 = i1+1; i2<m; i2++){

      ISSUE_VINST(hoist2_label);
      for(int j=0; j<n; j+=REGION_SIZE/2){
        
        for (int d = 0; d < vdim; d++){
          int vec1 = (i1+d)%m;
          int vec2 = (i2+d)%m;
          VPREFETCH_L(sp_data_offset, data + _idx_(vec1,j,n), d, REGION_SIZE/2,1); //vertical loads
          VPREFETCH_L(sp_data_offset+REGION_SIZE/2, data + _idx_(vec2,j,n), d, REGION_SIZE/2,1);
        }
        ISSUE_VINST(symmat_label);
        sp_data_offset = sp_data_offset + REGION_SIZE;
        if(sp_data_offset==NUM_REGIONS*REGION_SIZE)sp_data_offset=0;

      }
      ISSUE_VINST(i2_label);
    }
    ISSUE_VINST(i1_label);
  }
  


  //issue stack end portions of vector cores
  ISSUE_VINST(vector_stack_label);
  // devec with unique tag
  DEVEC(devec_0);

  //fence for all cores to ensure memory operations have completed
  asm volatile("fence\n\t");

  asm("trillium vissue_delim return scalar_return"); //return delimiter, delimiters can be of many types
  return;

  //all the vissue labels below:

  init_label: //this name matches with vissue label name
    asm("trillium glue_point init"); //name over here "init" matches with delimiter in vector code
  hoist1_label:
    asm("trillium glue_point hoist1"); 
  hoist2_label:
    asm("trillium glue_point hoist2"); 
  symmat_label:
    asm("trillium glue_point symmat");
  i2_label:
    asm("trillium glue_point i2");
  i1_label:
    asm("trillium glue_point i1");
  vector_stack_label: 
    asm("trillium glue_point vector_stack"); //name over here "vector_stack" matches with delimiter in vector code

  #elif defined VECTOR_CORE
  asm("trillium vissue_delim until_next init"); //until_next delimiter used, name (init) over here same as in glue point above
  //vector core code

  volatile int bh1,bh2,bh3;
  
  int sp_offset=0;
  DTYPE *spAddr = (DTYPE *)getSpAddr(ptid, 0);

  int i1=start+vtid;
  int i2=i1+1;
  int j=0;
  DTYPE symmat_temp=0;

  do{
    asm("trillium vissue_delim until_next hoist1");
    i2=i1+1;
    do {
      asm("trillium vissue_delim until_next hoist2");
      symmat_temp=0;
      j=0;
      do {
        asm("trillium vissue_delim until_next symmat");
        FRAME_START(REGION_SIZE);
        #pragma GCC unroll(8)
        for(int jj=0; jj<REGION_SIZE/2; jj++){
          symmat_temp+= spAddr[sp_offset+jj]*spAddr[sp_offset+REGION_SIZE/2+jj];
        }
        REMEM(REGION_SIZE);
        sp_offset += REGION_SIZE;
        // if(sp_offset==NUM_REGIONS)sp_offset=0;
        sp_offset = sp_offset%(NUM_REGIONS*REGION_SIZE);
        j+=REGION_SIZE;
      } while(bh3);

      asm("trillium vissue_delim until_next i2");
      int cond = (i2<m);
      volatile int compiler_hack = 1;
      PRED_EQ(cond,1);
      if(compiler_hack){
        symmat[i1*m+i2]=symmat_temp;
        symmat[i2*m+i1]=symmat_temp;
      }
      PRED_EQ(ptid,ptid);
      i2+=1;
    } while(bh2);
    asm("trillium vissue_delim until_next i1");
    i1+=stride;
  } while(bh1);

  asm("trillium vissue_delim return vector_stack"); //return delimiter
  return;
  #endif


}