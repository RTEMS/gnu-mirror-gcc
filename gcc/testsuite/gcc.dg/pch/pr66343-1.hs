/* PR sanitizer/66343 */
/* { dg-skip-if "sanitizers and capabilities are not supported together" { aarch64_capability_any } } */
/* { dg-options "-fsanitize=undefined" } */

void
foo (int a, int b)
{
  a / b;
}
