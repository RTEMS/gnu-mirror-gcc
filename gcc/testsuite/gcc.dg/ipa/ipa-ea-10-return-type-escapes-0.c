/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -fprint-cast-analysis" } */

#include <stddef.h>

struct astruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};
struct astruct_s astruct; // This should not escape
struct bstruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};
struct bstruct_s bstruct; // This should not escape

// This will make astruct_s escape
struct astruct_s __attribute__ ((externally_visible)) escaping ()
{
  struct astruct_s a;
  return a;
}
struct bstruct_s
non_escaping ()
{}

int
main ()
{
  astruct = escaping ();
  bstruct = non_escaping ();
}

// This says that astruct_s escapes because it is returning from an externally
// visible function
/* { dg-final { scan-wpa-ipa-dump " record astruct_s .boolean_type a;boolean_type b;boolean_type c;. reason: e=1 g=0 p=0 r=1 c=0 v=0 u=0 i=0" "type-escape-analysis" } } */
// This says that bstruct_s does not escape
/* { dg-final { scan-wpa-ipa-dump " record bstruct_s .boolean_type a;boolean_type b;boolean_type c;. reason: e=0 g=0 p=0 r=0 c=0 v=0 u=0 i=0" "type-escape-analysis" } } */
