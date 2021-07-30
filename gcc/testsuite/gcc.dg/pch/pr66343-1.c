/* PR sanitizer/66343 */
/* { dg-do assemble } */
/* { dg-skip-if "sanitizers and capabilities are not supported together" { aarch64_capability_any } } */
/* { dg-options "-fsanitize=undefined" } */

#include "pr66343-1.h"

void
bar (int a, int b)
{
  a / b;
}

/* Hack to turn off PCH assembly comparison, as it is incompatible
   with dg-do assemble.  The target condition will be always false.  */
/* { dg-error "" "" { target { lp64 && { ! lp64 } } } } */
