/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -fprint-access-analysis " } */

#include <stdio.h>

struct astruct_s
{
  float a;
  _Bool b;
  int c;
};
struct astruct_s astruct;

int
main ()
{
  printf ("%d\n", astruct.a);
  printf ("%d\n", astruct.c);
}

/* { dg-final { scan-ipa-dump " record astruct_s .real_type a.boolean_type b.integer_type c.. ..  record astruct_s .real_type a.integer_type c.." "type-escape-analysis" } } */
