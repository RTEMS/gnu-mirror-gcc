/* { dg-do link } */
/* { dg-options  "-fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -fprint-access-analysis " } */

#include <stdio.h>

struct astruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};
struct astruct_s *astruct;

int
main ()
{
  astruct->a++;
  astruct->a = 3;
}

// This says that only writes are happening even if astruct is a pointer
/* { dg-final { scan-wpa-ipa-dump "astruct_s.a = 0x0003" "type-escape-analysis" } } */
