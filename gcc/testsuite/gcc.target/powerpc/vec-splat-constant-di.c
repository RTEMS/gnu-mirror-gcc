/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test generating DImode constants that have the same bit pattern as DFmode
   constants that can be loaded with the XXSPLTIDP instruction with the ISA 3.1
   (power10).  We use asm to force the value into vector registers.  */

double
scalar_0 (void)
{
  double d;
  long long ll = 0;

  __asm__ ("xxmr %x0,%x1" : "=wa" (d) : "wa" (ll));
  return d;			/* XXSPLTIB or XXLXOR.  */
}

double
scalar_neg_0 (void)
{
  double d;
  long long ll = 0x80000000LL;

  __asm__ ("xxmr %x0,%x1" : "=wa" (d) : "wa" (ll));
  return d;			/* XXSPLTIDP.  */
}

double
scalar_large_value (void)
{
  double d;
  long long ll = 0x80000001LL;

  __asm__ ("xxmr %x0,%x1" : "=wa" (d) : "wa" (ll));
  return d;			/* PLI and MTVSRD.  */
}

/* { dg-final { scan-assembler-times {\mxxspltidp\M} 1 } } */
