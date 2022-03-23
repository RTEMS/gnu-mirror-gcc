/* { dg-do compile } */
/* { dg-options "-mprefetchi -O2" } */
/* { dg-final { scan-assembler-not "prefetchit0" } } */
/* { dg-final { scan-assembler-not "prefetchit1" } } */

#include <x86intrin.h>

void* p;

void extern
prefetchi_test (void)
{
  __builtin_prefetch (p, 0, 3, 0); /* { dg-warning "instruction prefetch applies when in 64-bit mode with RIP-relative addressing and option '-mprefetchi'; they stay NOPs otherwise" } */
  __builtin_prefetch (p, 0, 2, 0); /* { dg-warning "instruction prefetch applies when in 64-bit mode with RIP-relative addressing and option '-mprefetchi'; they stay NOPs otherwise" } */
}
