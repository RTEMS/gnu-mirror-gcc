/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

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
  _Bool *a = &astruct.a;
  _Bool *c = &astruct.c;
  _Bool *c_1 = a + 1;
  _Bool *a_1 = c - 1;
  assert (c_1 == c);
  assert (a_1 == a);
  return 0;
}
