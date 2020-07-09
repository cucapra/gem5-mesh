#ifndef __VECTOR_UTIL_H__
#define __VECTOR_UTIL_H__

int roundUp(int numToRound, int multiple);
inline int min(int a, int b) {
  if (a > b) {
    return b;
  }
  else {
    return a;
  }
}





#endif