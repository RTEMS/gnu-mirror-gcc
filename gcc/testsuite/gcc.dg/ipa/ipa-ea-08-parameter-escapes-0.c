/* { dg-do link } */
/* { dg-options "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -fprint-cast-analysis" } */
/* { dg-require-effective-target lto } */

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

__attribute__ ((externally_visible)) void
escaping (struct astruct_s cstruct)
{}
void
non_escaping (struct bstruct_s dstruct)
{}

int
main ()
{
  escaping (astruct);
  non_escaping (bstruct);
}

/* { dg-final { scan-wpa-ipa-dump " record astruct_s .boolean_type a;boolean_type b;boolean_type c;. reason: e=1 g=0 p=1 r=0 c=0 v=0 u=0 i=0" "type-escape-analysis" } } */
/* { dg-final { scan-wpa-ipa-dump " record bstruct_s .boolean_type a;boolean_type b;boolean_type c;. reason: e=0 g=0 p=0 r=0 c=0 v=0 u=0 i=0" "type-escape-analysis" } } */
