/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

#include <assert.h>

int
main ()
{
  struct astruct_s
  {
    _Bool _a;
    _Bool _b;
    _Bool _c;
    _Bool _d;
  };
  struct outerstruct_s
  {
    struct astruct_s a;
    struct astruct_s b;
    struct astruct_s c;
    struct astruct_s d;
  };
  struct outerstruct_s outerstruct;
  struct astruct_s a = outerstruct.a;
  struct astruct_s b = outerstruct.b;
  struct astruct_s c = outerstruct.c;
  struct astruct_s d = outerstruct.d;
  _Bool _a = a._a;
  _Bool _c = a._c;
  _Bool _d = a._d;
}

/* { dg-final { scan-ipa-dump "replacing field a 0 with a 0" "type-escape-analysis" } } */
/* { dg-final { scan-ipa-dump "replacing field b 32 with b 32" "type-escape-analysis" } } */
/* { dg-final { scan-ipa-dump "replacing field c 64 with c 64" "type-escape-analysis" } } */
/* { dg-final { scan-ipa-dump "replacing field d 96 with d 96" "type-escape-analysis" } } */
/* { dg-final { scan-ipa-dump "replacing field _a 0 with _a 0" "type-escape-analysis" } } */
/* { dg-final { scan-ipa-dump "replacing field _c 16 with _c 8" "type-escape-analysis" } } */
/* { dg-final { scan-ipa-dump "replacing field _d 24 with _d 16" "type-escape-analysis" } } */
