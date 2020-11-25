/* { dg-do link } */
/* { dg-options  "-fipa-type-escape-analysis -fdump-ipa-type-escape-analysis" } */

#include <stddef.h>

struct astruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};

void
record_parameter (struct astruct_s a)
{}
void
pointer_parameter (struct astruct_s *a)
{}
void
array_parameter (struct astruct_s a[])
{}

int
main ()
{}

// We have a complete type, probably from record_parameter
/// { dg-final { scan-wpa-ipa-dump "collected:  record astruct_s .boolean_type a.boolean_type b.boolean_type c.." "type-escape-analysis" } } */
// Now we have an incomplete struct
/// { dg-final { scan-wpa-ipa-dump "collected:  record astruct_s .." "type-escape-analysis" } } */
// This is the pointer...
/// { dg-final { scan-wpa-ipa-dump "collected:  record astruct_s ..." "type-escape-analysis" } } */
// We are missing the array parameter
// But it seems that the array parameter is passed as a pointer, which makes
// sense... array_parameter (struct astruct_s * a)
// {
//   <bb 2> :
//   return;
//
// }
