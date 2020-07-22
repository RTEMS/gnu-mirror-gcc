/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -fprint-cast-analysis" } */

#include <stddef.h>

struct astruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};
__attribute__ ((
  externally_visible)) struct astruct_s astruct; // This should escape
struct bstruct_s
{
  _Bool a;
  _Bool b;
  _Bool c;
};
struct bstruct_s bstruct; // This should not escape

int
main ()
{
  astruct.a = 0;
  bstruct.b = 0;
}

// This test is pretty much the same as the previous one.
// The reason for this is because initially the previous
// test tested the escaping of variables and this one of
// types. Since, we are now not checking variables escaping
// these two tests have merged.

/* { dg-final { scan-wpa-ipa-dump "collected:  record astruct_s .boolean_type a;boolean_type b;boolean_type c;." "type-escape-analysis" } } */
/* { dg-final { scan-wpa-ipa-dump "collected:  record bstruct_s .boolean_type a;boolean_type b;boolean_type c;." "type-escape-analysis" } } */
// Do we find the externally visible struct?
// This says that record astruct_s is escaping because the reason g (global is
// visible) is set to true...
/* { dg-final { scan-wpa-ipa-dump " record astruct_s .boolean_type a.boolean_type b.boolean_type c.. reason: e=1 g=1 p=0 r=0 c=0 v=0 u=0 i=0" "type-escape-analysis" } } */
// This says that record bstruct_s is not escaping
/* { dg-final { scan-wpa-ipa-dump " record bstruct_s .boolean_type a.boolean_type b.boolean_type c.. reason: e=0 g=0 p=0 r=0 c=0 v=0 u=0 i=0" "type-escape-analysis" } } */
