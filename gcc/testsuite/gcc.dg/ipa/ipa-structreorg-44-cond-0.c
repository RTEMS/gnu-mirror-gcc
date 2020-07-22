/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

struct a
{
  struct b *b;
} c (struct a *d)
{
  while (d)
    ;
}
void
main ()
{}

/* { dg-final { scan-ipa-dump "deleting all fields for struct a" "type-escape-analysis" } } */
