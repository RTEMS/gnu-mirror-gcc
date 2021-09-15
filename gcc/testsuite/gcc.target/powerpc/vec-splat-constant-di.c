/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test generating DImode constants that have the same bit pattern as DFmode
   constants that can be loaded with the XXSPLTIDP instruction with the ISA 3.1
   (power10).  We use asm to force the value into vector registers.  */

double
scalar_0 (void)
{
  /* XXSPLTIB or XXLXOR.  */
  double d;
  long long ll = 0;

  __asm__ ("xxmr %x0,%x1" : "=wa" (d) : "wa" (ll));
  return d;
}

double
scalar_1 (void)
{
  /* VSPLTISW/VUPKLSW or XXSPLTIB/VEXTSB2D.  */
  double d;
  long long ll = 1;

  __asm__ ("xxmr %x0,%x1" : "=wa" (d) : "wa" (ll));
  return d;
}

/* 0x8000000000000000LL is the bit pattern for -0.0, which can be generated
   with XXSPLTIDP.  */
double
scalar_float_neg_0 (void)
{
  /* XXSPLTIDP.  */
  double d;
  long long ll = 0x8000000000000000LL;

  __asm__ ("xxmr %x0,%x1" : "=wa" (d) : "wa" (ll));
  return d;
}

/* 0x3ff0000000000000LL is the bit pattern for 1.0 which can be generated with
   XXSPLTIDP.  */
double
scalar_float_1_0 (void)
{
  /* XXSPLTIDP.  */
  double d;
  long long ll = 0x3ff0000000000000LL;

  __asm__ ("xxmr %x0,%x1" : "=wa" (d) : "wa" (ll));
  return d;
}

/* 0x400921fb54442d18LL is the bit pattern for PI, which cannot be generated
   with XXSPLTIDP.  */
double
scalar_pi (void)
{
  /* PLXV.  */
  double d;
  long long ll = 0x400921fb54442d18LL;

  __asm__ ("xxmr %x0,%x1" : "=wa" (d) : "wa" (ll));
  return d;
}

/* { dg-final { scan-assembler-times {\mxxspltidp\M} 1 } } */
