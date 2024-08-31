#include <stdio.h>

/* This is
   a comment */
int main(int argc, char**argv) {
  int foo = 12; // also a comment \
  printf("still in comment");
  for (int/*d*/i = 0;i<10;++i) foo+=3*i;
  return -1;
}
