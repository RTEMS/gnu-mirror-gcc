/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

#include <assert.h>
#include <stdio.h>
#include <stddef.h>

int
main (int argc, char **argv)
{
  struct astruct_s
  {
    _Bool a;
    _Bool b;
    _Bool c;
    _Bool d;
  };
  struct astruct_s a[2];
  struct astruct_s *a_0 = &(a[0]);
  struct astruct_s *a_1 = a_0 + argc;
  ptrdiff_t d = a_1 - a_0;
  printf ("%d %d %d\n", a_0->a, a_0->c, a_0->d);
  printf ("%d\n", d);
}
