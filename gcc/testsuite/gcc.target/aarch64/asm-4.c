/* { dg-do compile { xfail aarch64_capability_any } } */
/* { dg-options "-O0" } */

int x;

void
f (void)
{
  asm volatile ("%a0" :: "X" (__builtin_extend_pointer (&x))); /* { dg-bogus "invalid address mode" "" { xfail aarch64_capability_any } } */
  /* { dg-bogus "invalid expression as operand" "" { xfail aarch64_capability_any } .-1 } */
}
