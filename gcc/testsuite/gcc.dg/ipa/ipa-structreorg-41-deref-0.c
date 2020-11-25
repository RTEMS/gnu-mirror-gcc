/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa " } */

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>

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
  struct astruct_s *t, copy;
  t = &astruct;
  t->a = true;
  t->c = true;
  copy = *t;
  printf ("%d %d", copy.a, copy.c);
  assert (astruct.a == true);
  assert (astruct.c == true);
  return 0;
}

/// { dg-final { scan-ipa-dump "replacing field a 0 with a 0" "type-escape-analysis" } } */
/// { dg-final { scan-ipa-dump "replacing field c 16 with c 8" "type-escape-analysis" } } */
