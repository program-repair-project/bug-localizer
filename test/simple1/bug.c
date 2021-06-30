#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int calc(int arg) {
  if (arg != 487) {
    return arg * arg;
  } else {
    return arg + 1;
  }
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Please, provide a positive float number.");
    return 1;
  }

  int arg = atoi(argv[1]);
  int res = calc(arg);

  printf("%d\n", res);
  return 0;
}
