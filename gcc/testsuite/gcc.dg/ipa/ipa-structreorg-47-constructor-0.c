/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

#include <assert.h>
#include <stddef.h>
#include <stdio.h>

int
main ()
{
  struct astruct_s
  {
    _Bool a;
    _Bool b;
    _Bool c;
  };
  struct astruct_s an_arc;
  struct another
  {
    _Bool a;
    struct astruct_s d;
    _Bool c;
  };
  struct another an_another = {0, {0, 1}, 1};
  struct astruct_s a = an_another.d;
  printf ("%d %d %d %d %d", an_another.a, &a, a.a, a.c, an_another.c);
}
