/* Dummy main program for 331 language code */
#include "inc.331.h"
#include <stdio.h>

extern int our_code_starts_here();

int main(int argc, char** argv) {

  int result = our_code_starts_here();
  printf("%d\n", result);
  return 0;
}
