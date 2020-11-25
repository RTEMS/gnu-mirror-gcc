/* { dg-do link } */
/* { dg-options  "-fipa-type-escape-analysis -fdump-ipa-type-escape-analysis" } */

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
  astruct.a++;
  astruct.a = 3;
}

// This means that this is only a write and not a read.
/// { dg-final { scan-wpa-ipa-dump "astruct_s.a = 0x0003" "type-escape-analysis" } } */
