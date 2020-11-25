/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa" } */

#include <stddef.h>

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
  _Bool d;
};

struct bstruct_s *
casting_to_void (struct astruct_s *s)
{
  return (struct bstruct_s *) (s);
}

int
main ()
{
  struct astruct_s astruct;
  struct bstruct_s bstruct;
  casting_to_void (&astruct);
}

// The type
/// { dg-final { scan-wpa-ipa-dump " record bstruct_s .boolean_type a;boolean_type b;boolean_type c;boolean_type d;. reason: e=1 g=0 p=0 r=0 c=1 v=0 u=0 i=0" "type-escape-analysis" } } */
/// { dg-final { scan-wpa-ipa-dump " record astruct_s .boolean_type a;boolean_type b;boolean_type c;. reason: e=1 g=0 p=0 r=0 c=1 v=0 u=0 i=0" "type-escape-analysis" } } */
// The pointer
/// { dg-final { scan-wpa-ipa-dump " record bstruct_s .boolean_type a;boolean_type b;boolean_type c;boolean_type d;.. reason: e=1 g=0 p=0 r=0 c=1 v=0 u=0 i=0" "type-escape-analysis" } } */
/// { dg-final { scan-wpa-ipa-dump " record astruct_s {boolean_type a;boolean_type b;boolean_type c;}* reason: e=1 g=0 p=0 r=0 c=1 v=0 u=0 i=0" "type-escape-analysis" } } */

// But there are incomplete types... should does be marked as escaping as well
// here? No, because at the moment, how we compute it is with the fixed point
// and at that moment, we get rid of reasons. So, at the moment, it is
// sufficient to say that only a fraction of equivalent types escape.
