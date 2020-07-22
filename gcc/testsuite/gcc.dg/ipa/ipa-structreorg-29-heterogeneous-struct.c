/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

#include <assert.h>
#include <stdio.h>
#include <stddef.h>

int
main ()
{
  struct astruct_s
  {
    int a;
    _Bool b;
    int c;
  };
  struct astruct_s astruct;
  int *a = &(astruct.a);
  int *c = &(astruct.c);
  ptrdiff_t d = c - a;
  assert (d == 1);
}
