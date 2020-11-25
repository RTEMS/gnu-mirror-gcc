/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa -fipa-dlo-tests " } */

#include <assert.h>

int
main ()
{
  struct astruct_s
  {
    _Bool a;
    _Bool b;
    _Bool c;
    _Bool d;
  };
  struct astruct_s astruct;
  struct astruct_s *p0 = &astruct;
  struct astruct_s **p1 = &p0;
  _Bool *a_ptr = &(astruct.a);
  _Bool *c_ptr = &(astruct.c);
  _Bool *c_ptr_from_1 = a_ptr + 1;
  _Bool *a_ptr_2 = &((*p1)->a);
  _Bool *c_ptr_from_2 = a_ptr_2 + 1;
  assert (c_ptr == c_ptr_from_1);
  assert (c_ptr == c_ptr_from_2);
}
