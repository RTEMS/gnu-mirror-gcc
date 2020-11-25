/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa " } */

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
  printf ("%d %d\n", astruct.a, astruct.c);
  return 0;
}

// This is different from previous because here we are replacing a local
// declaration
/// { dg-final { scan-ipa-dump "replacing field a 0 with a 0" "type-escape-analysis" } } */
/// { dg-final { scan-ipa-dump "replacing field c 16 with c 8" "type-escape-analysis" } } */
