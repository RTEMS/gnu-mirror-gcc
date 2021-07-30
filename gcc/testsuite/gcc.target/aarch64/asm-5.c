/* PR target/87598 */
/* { dg-do compile  { target { ! aarch64_capability_any } } } */

void
foo (void)
{
  __asm__ ("# %a0" : : "i" (0));
}
