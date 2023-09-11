/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O" "-O1" "-O2" "-O3" } } */
/* { dg-additional-options "--save-temps" } */

/* Noinline in order to make sure that the `memcpy` in this function does not
   get copied into `main` and break our scan-assembler-times.  */
__attribute__ ((noinline))
__uintcap_t foo (void *p)
{
  __uintcap_t n;
  __builtin_memcpy (&n, p, sizeof (n));
  return n;
}

unsigned long bar (void *p)
{
  unsigned long n;
  __builtin_memcpy (&n, p, sizeof (n));
  return n;
}

int main()
{
  /* Would very much like to *ensure* that the alignment of the structure
     on the stack is not aligned to 16.  The `memcpy` below should be good
     enough to ensure that we don't assume the __builtin_memcpy is aligned, but
     having this structure unaligned and requiring the program to run would be
     even better.  */
  struct {
      unsigned long x;
      unsigned long y;
      unsigned long z;
  } myval;
  myval.x = 0;
  myval.z = 0;
  __uintcap_t x = foo (&myval);
  unsigned long z = bar (&myval.z);
  return x + z;
}
/* { dg-final { scan-assembler-times "bl\tmemcpy" 1 { target  cheri_capability_pure  } } } */
