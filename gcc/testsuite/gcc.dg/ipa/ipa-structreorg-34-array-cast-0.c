/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa" } */

#include <assert.h>
#include <stdio.h>
#include <stddef.h>

int
main (int argc, char **argv)
{
  struct astruct_s
  {
    _Bool a;
    _Bool b;
    _Bool c;
    _Bool d;
  };
  struct astruct_s a[2][2];

  struct astruct_s b = a[argc][argc];
  printf ("%d %d %d\n", b.a, b.c, b.d);
}

/// { dg-final { scan-ipa-dump "replacing field a 0 with a 0" "type-escape-analysis" } } */
/// { dg-final { scan-ipa-dump "replacing field c 16 with c 8" "type-escape-analysis" } } */
/// { dg-final { scan-ipa-dump "replacing field d 24 with d 16" "type-escape-analysis" } } */
