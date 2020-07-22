/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis" } */

#include <stddef.h>
#include <assert.h>

int
main ()
{
  struct astruct_s
  {
    _Bool a;
    _Bool b;
    _Bool c;
  };
  struct astruct_s astruct;
  astruct.a = 1;
  astruct.c = 1;
  int d = astruct.a * astruct.c;
  assert (d == 1);
  _Bool *a = &(astruct.a);
  _Bool *c = &(astruct.c);
  assert (a + 1 == c);
  return 0;
}
