/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis" } */

#include <stddef.h>
#include <stdio.h>

int
main (int argc, char **argv)
{
  struct astruct_s
  {
    _Bool a;
    _Bool b;
    _Bool c;
  };
  volatile struct astruct_s astruct;
}

// This says that astruct_s has a volatile
/// { dg-final { scan-wpa-ipa-dump " record astruct_s .boolean_type a.boolean_type b.boolean_type c.. reason: e=1 g=0 p=0 r=0 c=0 v=1 u=0 i=0" "type-escape-analysis" } } */
