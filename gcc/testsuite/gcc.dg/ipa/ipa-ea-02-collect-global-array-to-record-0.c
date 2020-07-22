/* { dg-do link } */
/* { dg-options  "-fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -fprint-escape-analysis" } */

#include <stddef.h>

struct astruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};
struct astruct_s astruct[2];

int
main ()
{
  struct astruct_s another = astruct[0];
}

// This one is for the structure
/* { dg-final { scan-ipa-dump "collected:  record astruct_s .boolean_type a.boolean_type b.boolean_type c.." "type-escape-analysis" } } */
// This one is for the array. That's why it has two dots at the end. They correspond to []
/* { dg-final { scan-ipa-dump "collected:  record astruct_s .boolean_type a.boolean_type b.boolean_type c...." "type-escape-analysis" } } */
