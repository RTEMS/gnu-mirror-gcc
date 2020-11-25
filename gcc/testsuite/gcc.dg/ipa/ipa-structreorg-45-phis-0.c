/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa" } */

struct a
{
  struct a_inner *b;
} c (struct a *d, struct a *e)
{
  while (e)
    d = d;
}
int
main ()
{}

/// { dg-final { scan-ipa-dump "deleting all fields for struct a" "type-escape-analysis" } } */
