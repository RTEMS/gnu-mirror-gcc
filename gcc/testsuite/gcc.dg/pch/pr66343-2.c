/* PR sanitizer/66343 */
/* { dg-options "-fsanitize=undefined" } */
/* { dg-skip-if "sanitizers and capabilities are not supported together" { aarch64_capability_any } } */

#include "pr66343-2.h"

void
bar (int a, int b)
{
  a / b;
}
