/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa" } */

#include <stdio.h>

struct astruct_s
{
  float a;
  _Bool b;
  int c;
};
struct ostruct_s
{
  struct astruct_s *a;
  float b;
  float c;
};
struct ostruct_s ostruct;

int
main ()
{
  printf ("%d\n", ostruct.b);
  printf ("%d\n", ostruct.c);
  printf ("%f\n", ostruct.a->a);
  printf ("%d\n", ostruct.a->c);
}

/// { dg-final { scan-ipa-dump " record astruct_s ... ->  record astruct_s ..." "type-escape-analysis" } } */
/// { dg-final { scan-ipa-dump " record ostruct_s . record astruct_s ... a;real_type b;real_type c;. ->  record ostruct_s . record astruct_s ... a;real_type b;real_type c;." "type-escape-analysis" } } */
/// { dg-final { scan-ipa-dump " record astruct_s .real_type a;boolean_type b;integer_type c.. ->  record astruct_s .real_type a;integer_type c.." "type-escape-analysis" } } */
