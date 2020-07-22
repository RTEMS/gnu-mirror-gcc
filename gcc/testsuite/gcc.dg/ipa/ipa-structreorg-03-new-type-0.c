/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -fprint-access-analysis " } */

#include <stdio.h>

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
  printf ("%d\n", astruct.a);
  printf ("%d\n", astruct.c);
}

/* { dg-final { scan-ipa-dump " record astruct_s .boolean_type a;boolean_type b;boolean_type c;. ..  record astruct_s .boolean_type a;boolean_type c;." "type-escape-analysis" } } */
