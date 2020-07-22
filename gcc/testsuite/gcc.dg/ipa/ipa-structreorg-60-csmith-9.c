/* { dg-do run } */
/* { dg-options  "-w -flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

#include <stdint.h>

void
foo (uint64_t a, uint64_t b)
{
  a + b;
}

struct a
{
  uint64_t b;
  uint8_t c
} d ()
{
  // I think the problem here is with the const attribute...
  const struct a e;
  foo (0, e.b);
  return e;
}

main () {}

/* { dg-final { scan-ipa-dump "a.c may be deleted" "type-escape-analysis" } } */
