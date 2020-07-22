/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis" } */

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
  return 0;
}

/* { dg-final { scan-ipa-dump "astruct_s.a may be deleted" "type-escape-analysis" } } */
/* { dg-final { scan-ipa-dump "astruct_s.b may be deleted" "type-escape-analysis" } } */
/* { dg-final { scan-ipa-dump "astruct_s.c may be deleted" "type-escape-analysis" } } */
