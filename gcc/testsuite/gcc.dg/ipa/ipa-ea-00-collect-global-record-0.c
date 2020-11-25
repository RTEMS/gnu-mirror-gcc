/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa" } */

struct astruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};
struct astruct_s astruct;

int
main ()
{
  astruct.a = 0;
}

// Please note that braces and semicollons are replaced with dots in order
// to parse correctly
///// { dg-final { scan-ipa-dump "collected:  record astruct_s .boolean_type a.boolean_type b.boolean_type c.." "type-escape-analysis" } } */
