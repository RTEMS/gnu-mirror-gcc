/* { dg-do run } */
/* { dg-options  "-w -flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

#include <stdint.h>
// TODO: So, our analysis says that we are deleting a field "a".
// And that the field "a" is contained in struct "b".
// However, we are doing is_interesting_struct("c") == yes
// is_interesting_field("a") == yes
// and so we delete field a from struct c.
struct
{
  uint64_t a
} b[];
struct
{
  unsigned : 5;
  unsigned a
} c;
d ()
{
  uint16_t e = b;
  int8_t f = c.a;
}
main () {}
