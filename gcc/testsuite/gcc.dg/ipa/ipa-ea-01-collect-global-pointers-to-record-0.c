/* { dg-do link } */
/* { dg-options  "-fipa-type-escape-analysis -fdump-ipa-type-escape-analysis" } */

#include <stddef.h>

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
  astruct = NULL;
}

// This is for the structure
/* //// { dg-final { scan-ipa-dump "collected:  record astruct_s .boolean_type a.boolean_type b.boolean_type c.." "type-escape-analysis" } } */
// This one has an extra dot to mark the * for the pointer
/* /// { dg-final { scan-ipa-dump "collected:  record astruct_s .boolean_type a.boolean_type b.boolean_type c..." "type-escape-analysis" } } */
