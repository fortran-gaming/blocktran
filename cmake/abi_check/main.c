#include <stdio.h>
#include <stdlib.h>

#include "abi_check.h"

int main(void) {

  if(addone(2) != 3) {
    fprintf(stderr, "2 + 1 != %d", addone(2));
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
