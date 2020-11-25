/* { dg-do link } */
/* { dg-options  "-fipa-type-escape-analysis -fdump-ipa-type-escape-analysis" } */

#include <stddef.h>

struct astruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};
struct outer_struct
{
  _Bool d;
  struct astruct_s a;
};
struct outer_struct bstruct;

int
main ()
{
  bstruct.d = 0;
}

// We only care about collecting the inner struct for this test
/// { dg-final { scan-ipa-dump "collected:  record astruct_s .boolean_type a.boolean_type b.boolean_type c.." "type-escape-analysis" } } */
