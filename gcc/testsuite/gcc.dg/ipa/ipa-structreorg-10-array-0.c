/* { dg-do run } */
/* { dg-options  " -flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa -fipa-dlo-tests " } */

#include <assert.h>
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
  struct astruct_s a;
  printf ("%d %d\n", a.a, a.c);
  struct astruct_s b[2];
  a = b[0];
  _Bool *a_ptr = &a.a;
  _Bool *c_ptr = &a.c;
  assert (a_ptr + 1 == c_ptr);
}
