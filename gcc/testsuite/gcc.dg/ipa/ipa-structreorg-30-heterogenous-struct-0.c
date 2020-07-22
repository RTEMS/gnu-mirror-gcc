/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

#include <assert.h>
#include <stdio.h>
#include <stddef.h>

int
main ()
{
  // unmodified { a = 1, b = 4; c = 5; d = 8; e = 12
  // modified { a = 1, c = 2; d = 4, e = 8
  struct astruct_s
  {
    _Bool a;
    int b;
    _Bool c;
    int d;
    _Bool e;
  };
  struct astruct_s astruct;
  _Bool *a = &(astruct.a);
  printf ("%d %d\n", astruct.c, astruct.d);
  _Bool *e = &(astruct.e);
  ptrdiff_t diff = e - a;
  assert (diff == 8);
}
