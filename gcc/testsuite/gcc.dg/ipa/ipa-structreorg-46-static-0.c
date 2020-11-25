/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa -fipa-dlo-tests " } */

#include <assert.h>
#include <stddef.h>

struct a_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};

struct a_s a_t;

int
main ()
{
  _Bool *a = &(a_t.a);
  _Bool *c = &(a_t.c);
  ptrdiff_t diff = c - a;
  assert (diff == 1);
}
