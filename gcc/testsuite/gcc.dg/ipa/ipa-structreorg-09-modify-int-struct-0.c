/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa -fipa-dlo-tests" } */

#include <assert.h>

int
main ()
{
  struct astruct_s
  {
    int a;
    int b;
    int c;
    int d;
  };
  struct astruct_s astruct;
  int *a = &(astruct.a);
  int *c = &(astruct.c);
  int *d = &(astruct.d);
  assert (c - 1 == a);
  assert (a + 2 == d);
}
