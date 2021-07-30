/* PR sanitizer/66343 */
/* { dg-options "-fsanitize=undefined" } */
/* { dg-skip-if "sanitizers and capabilities are not supported together" { aarch64_capability_any } } */

void
foo (int a, int b)
{
  a / b;
}
