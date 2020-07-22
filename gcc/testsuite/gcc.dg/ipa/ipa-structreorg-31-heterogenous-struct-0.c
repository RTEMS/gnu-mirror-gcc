/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

#include <assert.h>
#include <stdio.h>
#include <stddef.h>

int
main ()
{
  // unmodified a = 0, d = 5; e = 6; f = 7; b = 8; c = 9
  // modified a = 0, d = 1, e = 2, f = 3, c = 4;
  struct astruct_s
  {
    _Bool a;
    _Bool d;
    _Bool e;
    _Bool f;
    _Bool b;
    int c;
  };
  struct astruct_s astruct;
  struct astruct_s *p = &astruct;
  printf ("%d %d %d %d %d\n", p->a, p->d, p->e, p->f, p->c);
  _Bool *a = &(p->a);
  int *c = &(p->c);
  ptrdiff_t d = (_Bool *) c - a;
  printf ("%d\n", d);
  assert (d == 4);
}
