/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

#include <assert.h>
#include <stdio.h>
#include <stddef.h>

struct astruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
  _Bool d;
};

// PASS BY VALUE
_Bool
foo (struct astruct_s astruct)
{
  _Bool *a = &astruct.a;
  assert (!*a);
  _Bool *c = a + 1;
  assert (*c);
  _Bool *d = a + 2;
  assert (*d);
  return *c;
}

int
main (int argc, char **argv)
{
  struct astruct_s astruct;
  astruct.a = 0;
  astruct.c = argc;
  astruct.d = 1;
  printf ("%d %d %d\n", astruct.a, astruct.c, astruct.d);
  foo (astruct);
}

/* { dg-final { scan-ipa-dump "replacing field a 0 with a 0" "type-escape-analysis" } } */
/* { dg-final { scan-ipa-dump "replacing field c 16 with c 8" "type-escape-analysis" } } */
/* { dg-final { scan-ipa-dump "replacing field d 24 with d 16" "type-escape-analysis" } } */
