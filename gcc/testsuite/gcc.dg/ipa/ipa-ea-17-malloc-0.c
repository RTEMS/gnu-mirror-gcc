/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -fprint-cast-analysis -Wno-incompatible-pointer-types" } */

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

struct astruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};

int
main (int argc, char **argv)
{
  struct astruct_s *a = malloc (sizeof (struct astruct_s));
}

// This says that astruct_s is not casted
/* { dg-final { scan-wpa-ipa-dump " record astruct_s .boolean_type a;boolean_type b;boolean_type c;. reason: e=0 g=0 p=0 r=0 c=0 v=0 u=0 i=0" "type-escape-analysis" } } */
/* { dg-final { scan-wpa-ipa-dump " record astruct_s .boolean_type a;boolean_type b;boolean_type c;.. reason: e=0 g=0 p=0 r=0 c=0 v=0 u=0 i=0"
"type-escape-analysis" } } */
