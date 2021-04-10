#include "corr_kernel.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

#ifdef _VEC

inline int _idx_(int y, int x, int width)
{
  return (y * width) + x;
}

#ifdef OPTIMIZED_TRANSPOSE

inline void prefetch_data_frame (DTYPE* data, int i, int j, int n, int vdim, int *sp_data_offset){
  for (int d = 0; d < vdim; d++){
    VPREFETCH_L(*sp_data_offset, data + _idx_(i+d,j,n), d, REGION_SIZE,TO_ONE_CORE); //vertical loads
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

  int prefetch_stride = REGION_SIZE;
  int startOffset = INIT_FRAMES*prefetch_stride;

  for (int i = start; i < end; i+=vdim){
    ISSUE_VINST(hoist1_label);
    //mean
    PREFETCH_VISSUE(mean_label)

    ISSUE_VINST(hoist2_label);
    //stdev
    PREFETCH_VISSUE(stddev_label)

    ISSUE_VINST(hoist3_label);
    //center
    PREFETCH_VISSUE(center_label)

    
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

  DTYPE one = 1;
  // double eps = 0.1;

  do{
    asm("trillium vissue_delim until_next hoist1");
    mean_temp=0;
    do{
      asm("trillium vissue_delim until_next mean");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(16)
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
    FSTORE_NOACK(mean_temp, mean + i, 0);
    // mean[i]=mean_temp;

    stddev_temp=0;
    do {
      asm("trillium vissue_delim until_next stddev");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(16)
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
    SQRTF_UNSAFE(stddev_temp, stddev_temp);

    // int cond = stddev_temp <= eps;
    // float dummy;// = stddev_temp;
    // asm volatile(                                                         
    //   ".insn r 0x33, 0x7, 0x5, x0, %[c0], %[c1]\n\t"                      
    //   "fmv.s %[dest], %[src]\n\t"
    //   ".insn r 0x33, 0x7, 0x5, x0, x0, x0\n\t"
    //   : [dest] "=f" (dummy)
    //   : [c0] "r" (cond), [c1] "r" (1), [src] "f" (1.0f));     

    // stddev_temp = dummy;

    // volatile int cond2 = stddev_temp <= eps; //redundant but here to avoid some compiler movement which causes stddev to get doubly sqrted
    int cond = stddev_temp <= eps;
    volatile int compiler_hack = 1;
    PRED_EQ(cond,1);
    if(compiler_hack){
      stddev_temp = 1.0;
    }
    PRED_EQ(ptid,ptid);

    


    FSTORE_NOACK(stddev_temp, stddev + i, 0);
    // stddev[i] = stddev_temp;
    

    j=0;
    do {
      asm("trillium vissue_delim until_next center");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(16)
      for(int jj=0; jj<REGION_SIZE; jj++){
        data_temp= (spAddr[sp_offset+jj]-mean_temp);
        float nsqrt;
        // asm volatile ("fsqrt.s	%[dest], %[src]\n\t" : 
        //   [dest] "=f" (nsqrt) : [src] "f" ((float)n));
        SQRTF_UNSAFE(nsqrt, (float)n);

        data_temp/=(nsqrt*stddev_temp);
        FSTORE_NOACK(data_temp, data + i*n+j+jj, 0);
        // data[i*n+j+jj]= data_temp;
      }
      REMEM(REGION_SIZE);
      sp_offset += REGION_SIZE;
      // if(sp_offset==NUM_REGIONS)sp_offset=0;
      sp_offset = sp_offset%(NUM_REGIONS*REGION_SIZE);
      j+=REGION_SIZE;
    } while(bh2);
    asm("trillium vissue_delim until_next symmat1");
    FSTORE_NOACK(one, symmat + i*m+i, 0);
    // symmat[i*m+i]=1; //make diagonal 1 for the vectors it is assigned
    i+=vdim;
  } while(bh1);

  asm("trillium vissue_delim return vector_stack"); //return delimiter
  return;
  #endif

}

inline void prefetch_corr2(DTYPE *data, int i1, int i2, int j, int m, int n, int vdim, int *sp_data_offset) {

  for (int d = 0; d < vdim; d++){
    int vec1 = (i1+d)%m;
    int vec2 = (i2+d)%m;
    VPREFETCH_L(*sp_data_offset, data + _idx_(vec1,j,n), d, REGION_SIZE_K2/2,TO_ONE_CORE);
    VPREFETCH_L(*sp_data_offset+REGION_SIZE_K2/2, data + _idx_(vec2,j,n), d, REGION_SIZE_K2/2,TO_ONE_CORE);
  }

  *sp_data_offset = *sp_data_offset + REGION_SIZE_K2;
  if(*sp_data_offset==NUM_REGIONS_K2*REGION_SIZE_K2)*sp_data_offset=0;
}

void tril_corr_vec_2(int mask, DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int stride, int vtid, int vdim, int ptid){


  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  //---------------------------------
  //scalar core code iterspersed with vissue
  ISSUE_VINST(init_label); //eg: this block will deal with initila stack manipulation and initilaization of variables
  //-----------------------------------
  //prefetch variables
  int spadRegion = 0;
  int sp_data_offset=0;

  int prefetch_stride = REGION_SIZE_K2/2;
  int startOffset = INIT_FRAMES*prefetch_stride;

  for (int i1 = start; i1 < m-1; i1+=stride){
    ISSUE_VINST(hoist1_label);
    for(int i2 = i1+1; i2<m; i2++){

      ISSUE_VINST(hoist2_label);

      for (int j = 0; j < startOffset; j+=prefetch_stride) {
        prefetch_corr2(data, i1, i2, j, m, n, vdim, &sp_data_offset);
      }

      for(int j=startOffset; j<n; j+=prefetch_stride){        
        prefetch_corr2(data, i1, i2, j, m, n, vdim, &sp_data_offset);
        ISSUE_VINST(symmat_label);
      }

      for (int j = n - startOffset; j < n; j+=prefetch_stride) {
        ISSUE_VINST(symmat_label);
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
        FRAME_START(REGION_SIZE_K2);
        #pragma GCC unroll(16)
        for(int jj=0; jj<REGION_SIZE_K2/2; jj++){
          symmat_temp+= spAddr[sp_offset+jj]*spAddr[sp_offset+REGION_SIZE_K2/2+jj];
        }
        REMEM();
        sp_offset += REGION_SIZE_K2;
        // if(sp_offset==NUM_REGIONS_K2)sp_offset=0;
        sp_offset = sp_offset%(NUM_REGIONS_K2*REGION_SIZE_K2);
        j+=REGION_SIZE_K2;
      } while(bh3);

      asm("trillium vissue_delim until_next i2");
      int cond = (i2<m);
      volatile int compiler_hack = 1;
      PRED_EQ(cond,1);
      if(compiler_hack){
        FSTORE_NOACK(symmat_temp, symmat + i1*m+i2, 0);
        // symmat[i1*m+i2]=symmat_temp;
        FSTORE_NOACK(symmat_temp, symmat + i2*m+i1, 0);
        // symmat[i2*m+i1]=symmat_temp;
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

#elif defined POLYBENCH_VERSION

inline void prefetch_data_frame (DTYPE* data, int i, int j, int m, int vdim, int *sp_data_offset){
  for (int u = 0; u < REGION_SIZE; u++) {
    VPREFETCH_LR((*sp_data_offset) + u, data + (i+u)*m+j, 0, 1,TO_ALL_CORES); //horiz loads
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

  int prefetch_stride = REGION_SIZE;
  int startOffset = INIT_FRAMES*prefetch_stride;

  for (int j = start; j < end; j+=vdim){
    ISSUE_VINST(hoist1_label);
    //mean
    PREFETCH_VISSUE(mean_label)

    ISSUE_VINST(hoist2_label);
    //stdev
    PREFETCH_VISSUE(stddev_label)

    ISSUE_VINST(hoist3_label);
    //center
    PREFETCH_VISSUE(center_label)

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

  int j=start+vtid;
  int i=0;
  DTYPE mean_temp=0;
  DTYPE stddev_temp=0;
  DTYPE data_temp;
  DTYPE one = 1;
  // double eps = 0.1;

  do{
    asm("trillium vissue_delim until_next hoist1");
    mean_temp=0;
    do{
      asm("trillium vissue_delim until_next mean");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(16)
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
    STORE_NOACK(mean_temp, mean + j, 0);

    stddev_temp=0;
    do {
      asm("trillium vissue_delim until_next stddev");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(16)
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
    FSTORE_NOACK(stddev_temp, stddev + j, 0);
    

    i=0;
    do {
      asm("trillium vissue_delim until_next center");
      FRAME_START(REGION_SIZE);
      #pragma GCC unroll(16)
      for(int jj=0; jj<REGION_SIZE; jj++){
        data_temp= (spAddr[sp_offset+jj]-mean_temp);
        data_temp/=(sqrt(n)*stddev_temp);
        FSTORE_NOACK(data_temp, data + (i+jj)*m+j, 0);
      }
      REMEM(REGION_SIZE);
      sp_offset += REGION_SIZE;
      // if(sp_offset==NUM_REGIONS)sp_offset=0;
      sp_offset = sp_offset%(NUM_REGIONS*REGION_SIZE);
      i+=REGION_SIZE;
    } while(bh2);
    asm("trillium vissue_delim until_next symmat1");
    FSTORE_NOACK(one, symmat + j*m+j, 0);
    // symmat[j*m+j]=1;
    j+=vdim;
  } while(bh1);

  asm("trillium vissue_delim return vector_stack"); //return delimiter
  return;
  #endif

}

inline void prefetch_corr2(DTYPE *data, int j1, int j2, int i, int m, int n, int vdim, int *sp_data_offset) {

  for (int u = 0; u < REGION_SIZE; u++) {
    VPREFETCH_LR((*sp_data_offset) + u, data + (i+u)*m+j1, 0, 1, TO_ALL_CORES); //j1
    VPREFETCH_LR((*sp_data_offset) + REGION_SIZE_K2/2 + u, data + (i+u)*m+j2, 0, 1, TO_ALL_CORES); //j2
  }

  *sp_data_offset = *sp_data_offset + REGION_SIZE_K2;
  if(*sp_data_offset==NUM_REGIONS_K2*REGION_SIZE_K2)*sp_data_offset=0;
}

void tril_corr_vec_2(int mask, DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int stride, int vtid, int vdim, int ptid){


  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  //---------------------------------
  //scalar core code iterspersed with vissue
  ISSUE_VINST(init_label); //eg: this block will deal with initila stack manipulation and initilaization of variables
  //-----------------------------------
  //prefetch variables
  int spadRegion = 0;
  int sp_data_offset=0;

  int prefetch_stride = REGION_SIZE_K2/2;
  int startOffset = INIT_FRAMES*prefetch_stride;

  for (int j1 = start; j1 < m-1; j1+=stride){
    ISSUE_VINST(hoist1_label);
    for(int j2 = j1+1; j2<m; j2++){

      ISSUE_VINST(hoist2_label);

      for (int i = 0; i < startOffset; i+=prefetch_stride) {
        prefetch_corr2(data, j1, j2, i, m, n, vdim, &sp_data_offset);
      }

      for(int i=startOffset; i<n; i+=prefetch_stride){        
        prefetch_corr2(data, j1, j2, i, m, n, vdim, &sp_data_offset);
        ISSUE_VINST(symmat_label);
      }

      for (int i = 0; i < startOffset; i+=prefetch_stride) {
        ISSUE_VINST(symmat_label);
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

  int j1=start+vtid;
  int j2=j1+1;
  int i=0;
  DTYPE symmat_temp=0;

  do{
    asm("trillium vissue_delim until_next hoist1");
    j2=j1+1;
    do {
      asm("trillium vissue_delim until_next hoist2");
      symmat_temp=0;
      i=0;
      do {
        asm("trillium vissue_delim until_next symmat");
        FRAME_START(REGION_SIZE_K2);
        #pragma GCC unroll(16)
        for(int jj=0; jj<REGION_SIZE_K2/2; jj++){
          symmat_temp+= spAddr[sp_offset+jj]*spAddr[sp_offset+REGION_SIZE_K2/2+jj];
        }
        REMEM();
        sp_offset += REGION_SIZE_K2;
        // if(sp_offset==NUM_REGIONS_K2)sp_offset=0;
        sp_offset = sp_offset%(NUM_REGIONS_K2*REGION_SIZE_K2);
        i+=REGION_SIZE_K2;
      } while(bh3);

      asm("trillium vissue_delim until_next i2");
      int cond = (j2<m);
      volatile int compiler_hack = 1;
      PRED_EQ(cond,1);
      if(compiler_hack){
        FSTORE_NOACK(symmat_temp, symmat + j1*m+j2, 0);
        // symmat[i1*m+i2]=symmat_temp;
        FSTORE_NOACK(symmat_temp, symmat + j2*m+j1, 0);
        // symmat[i2*m+i1]=symmat_temp;
      }
      PRED_EQ(ptid,ptid);
      j2+=1;
    } while(bh2);
    asm("trillium vissue_delim until_next i1");
    j1+=stride;
  } while(bh1);

  asm("trillium vissue_delim return vector_stack"); //return delimiter
  return;
  #endif


}
#endif

#endif