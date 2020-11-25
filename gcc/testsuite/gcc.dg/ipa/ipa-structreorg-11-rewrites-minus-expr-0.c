/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa -fipa-dlo-tests" } */

#include <assert.h>
#include <stddef.h>

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
  _Bool *c_ptr = &(astruct.c);
  _Bool *a_ptr = &(astruct.a);
  ptrdiff_t diff = c_ptr - a_ptr;
  assert (diff == 1);
  return 0;
}
