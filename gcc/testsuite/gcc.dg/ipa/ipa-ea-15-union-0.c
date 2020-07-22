/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -fprint-cast-analysis" } */

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
  union outer
  {
    struct astruct_s a;
    double b;
  };
  union outer an_outer;
}

// This says that astruct_s is inside a union
/* { dg-final { scan-wpa-ipa-dump " record astruct_s {boolean_type a;boolean_type b;boolean_type c;} reason: e=1 g=0 p=0 r=0 c=0 v=0 u=1 i=0" "type-escape-analysis" } } */
