/* { dg-do link } */
/* { dg-options  "-flto -flto-partition=none -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa" } */

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
  if (astruct.a)
    {
      puts ("hello world");
    }
}

// This says that astruct_s.a is read in a conditional
//// { dg-final { scan-wpa-ipa-dump "astruct_s.a = 0x0001" "type-escape-analysis" } } */
