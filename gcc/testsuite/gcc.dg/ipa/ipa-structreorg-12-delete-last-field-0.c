/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa -fipa-dlo-tests " } */

#include <assert.h>

int
main ()
{
  struct astruct_s
  {
    _Bool a;
    _Bool c;
    _Bool b;
  };
  struct astruct_s b[2];
  _Bool *first_of_second = &(b[1].a);
  _Bool *c_0 = &(b[0].c);
  _Bool *a_1 = &(b[1].a);
  _Bool *a_1_from_c_0 = c_0 + 1;
  _Bool test = a_1_from_c_0 == a_1;
  char compile_test[test ? 1 : -1];
  assert (test);
}
