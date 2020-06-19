#ifndef __TEMP_H__
#define __TEMP_H__

#define _VEC
#define VEC_LEN 4 //vec group size


#define REGION_SIZE 8 //configure using LCM of required frame/region sizes
#define NUM_REGIONS (512 / REGION_SIZE) // (0,512) in this case is the hardware region area 

#define TOKEN_LEN 8 //how many elements get reduced together in tree

typedef int DTYPE;

// pthread argument for the kernel
typedef struct Kern_Args
{
  DTYPE *a;
  int n;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, int n,
    int tid_x, int tid_y, int dim_x, int dim_y);

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, int n,
    int tid_x, int tid_y, int dim_x, int dim_y);

#endif
