#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "pred.h"
// #include "../../common/bind_defs.h"

int main(int argc, char *argv[]) {
  
  /*--------------------------------------------------------------------
   * Setup scratchpads
   *------------------------------------------------------------------*/ 
  
  initScratchpads();

  /*--------------------------------------------------------------------
  * Run the test
  *-------------------------------------------------------------------*/  
  
  kernel(NULL, NULL, NULL, 0, 0, 0, 1, 1);
  
  return 0;
}
