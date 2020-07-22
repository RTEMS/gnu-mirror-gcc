/* { dg-do run } */
/* { dg-options  "-w -flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

#include <stdint.h>
struct a
{
  int32_t b
} c;
d ()
{
  for (;; c.b = 0)
    ;
}
main () {}
/* { dg-final { scan-ipa-dump "deleting field b 0" "type-escape-analysis" } } */
