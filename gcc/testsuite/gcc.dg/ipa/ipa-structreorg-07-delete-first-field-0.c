/* { dg-do run } */
/* { dg-options  "-w -flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa -fipa-dlo-tests" } */

#include <assert.h>
#include <stdio.h>

int
main ()
{
  struct astruct_s
  {
    _Bool b;
    _Bool a;
    _Bool c;
    _Bool d;
  };
  struct astruct_s astruct;

  printf ("%d %d %d\n", astruct.a, astruct.c, astruct.d);
  _Bool *a_ptr = &astruct.a;
  struct astruct_s *s_ptr = &astruct;
  assert (a_ptr == s_ptr);
}
