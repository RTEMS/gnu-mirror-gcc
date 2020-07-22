/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -fprint-cast-analysis -Wno-incompatible-pointer-types" } */

#include <stddef.h>
#include <stdio.h>

struct astruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};
struct bstruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};
void
foo (struct bstruct_s *s){};

int
main (int argc, char **argv)
{
  struct astruct_s astruct;
  foo (&astruct);
}

// This says that astruct_s is casted
/* { dg-final { scan-wpa-ipa-dump " record astruct_s .boolean_type a;boolean_type b;boolean_type c;. reason: e=1 g=0 p=0 r=0 c=1 v=0 u=0 i=0"
"type-escape-analysis" } } */
// This says that the pointer is casted
/* { dg-final { scan-wpa-ipa-dump " record astruct_s .boolean_type a;boolean_type b;boolean_type c;.. reason: e=1 g=0 p=0 r=0 c=1 v=0 u=0 i=0" "type-escape-analysis" } } */
