/* { dg-do compile { target cheri_capability_pure } } */

/* The below memcpy operation needs to preserve capabilities.
   We can not assume that `x` is aligned, and hence we must emit a memcpy (or a
   fully inlined version with runtime checks for alignment).  */
void *f(int **x)
{
  int *y;
  __builtin_memcpy(&y, x, 16);
  return y;
}

/* { dg-final { scan-assembler-times "bl\tmemcpy" 1 } } */
