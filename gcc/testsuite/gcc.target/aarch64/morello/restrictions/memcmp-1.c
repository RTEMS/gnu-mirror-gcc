/* Taken from ASAN testsuite.  */
/* { dg-do run } */
/* { dg-options "-fno-builtin-memcmp" } */
/* { dg-shouldfail-purecap "morello bounds" } */

#include <string.h>

volatile int one = 1;

int
main ()
{
  char a1[] = {(char)one, 2, 3, 4};
  char a2[] = {1, (char)(2*one), 3, 4};
  int res = memcmp (a1, a2, 5 + one);
  return res;
}
