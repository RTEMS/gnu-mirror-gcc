/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa " } */

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
  _Bool a = astruct.a;
  _Bool c = astruct.c;
  return 0;
}

/// { dg-final { scan-ipa-dump "replacing field a 0 with a 0" "type-escape-analysis" } } */
/// { dg-final { scan-ipa-dump "replacing field c 16 with a 8" "type-escape-analysis" } } */
