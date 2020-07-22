/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

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
  struct astruct_s astruct;
  astruct.c = 0;
  printf ("%d %d\n", astruct.a, astruct.c);
  return 0;
}

/* { dg-final { scan-ipa-dump "replacing field a 0 with a 0" "type-escape-analysis" } } */
// There's two... mmm.. not sure how to convey this here to the test?
/* { dg-final { scan-ipa-dump "replacing field c 16 with c 8" "type-escape-analysis" } } */
/* { dg-final { scan-ipa-dump "replacing field c 16 with c 8" "type-escape-analysis" } } */
