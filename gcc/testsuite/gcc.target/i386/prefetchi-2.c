/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-mprefetchi -fpie -O2" } */
/* { dg-final { scan-assembler-not "\[ \\t\]+prefetchit0" } } */
/* { dg-final { scan-assembler-not "\[ \\t\]+prefetchit1" } } */

#include <x86intrin.h>

int
bar (int a)
{
  return a + 1;
}

int
foo1 (int b)
{
  __builtin_prefetch (bar, 0, 3, 0); /* { dg-warning "instruction prefetch applies when in 64-bit mode with RIP-relative addressing and option '-mprefetchi'; they stay NOPs otherwise" } */
  return bar (b) + 1;
}

int
foo2 (int b)
{
  __builtin_prefetch (bar, 0, 2, 0); /* { dg-warning "instruction prefetch applies when in 64-bit mode with RIP-relative addressing and option '-mprefetchi'; they stay NOPs otherwise" } */
  return bar (b) + 1;
}
