#include <stdio.h>
#include <stdlib.h>

/*
echo src/PE/p15.c | entr sh -c 'cc -o p15 src/PE/p15.c && ./p15 1000'
*/

int main(int argc, char *argv[]) {
  int tp = atoi(argv[1]);
  char result[tp/2];
  printf("INIT: 2 to the power of %d\n", tp);
  for (int i = 0; i < tp; i++) result[i] = 0;
  int place = 0;
  result[place] = 1;
  int carry = 0;
  for (int p = 0; p < tp; p++) {
    for (int i = 0; i <= place; i++) {
      int d2 = (result[i] << 1) + carry;
      if (d2 >= 10) {
        carry = d2 / 10;
        d2 = d2 % 10;
        if (i == place) place++;
      } else {
        carry = 0;
      }
      result[i] = d2;
      // printf ("#> i: %d, p: %d, place: %d, tp: %d, [%2d]->[%2d](%2d)\n", i, p, place, tp, d, d2, carry);
    }
  }
  printf("#> RESULT: (2 power %d): \t", tp);
  int sum = 0;
  for (int i = place; i >= 0; i--) {
    printf ("%1d", result[i]);
    sum += result[i];
  } printf("\n");
  printf("#> SUM of digits: %d\n", sum);
  return 0;
}
