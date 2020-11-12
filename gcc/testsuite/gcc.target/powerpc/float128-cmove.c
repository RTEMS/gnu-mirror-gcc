/* { dg-do compile } */
/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */
/* { dg-final { scan-assembler     {\mxscmpeq[dq]p\M} } } */
/* { dg-final { scan-assembler     {\mxxpermdi\M}     } } */
/* { dg-final { scan-assembler     {\mxxsel\M}        } } */
/* { dg-final { scan-assembler-not {\mxscmpu[dq]p\M}  } } */
/* { dg-final { scan-assembler-not {\mfcmp[uo]\M}     } } */
/* { dg-final { scan-assembler-not {\mfsel\M}         } } */

/* This series of tests tests whether you can do a conditional move where the
   test is one floating point type, and the result is another floating point
   type.

   If the comparison type is SF/DFmode, and the move type is IEEE 128-bit
   floating point, we have to duplicate the mask in the lower 64-bits with
   XXPERMDI because XSCMPEQDP clears the bottom 64-bits of the mask register.

   Going the other way (IEEE 128-bit comparsion, 64-bit move) is fine as the
   mask word will be 128-bits.  */

float
eq_f_d (float a, float b, double x, double y)
{
  return (x == y) ? a : b;
}

double
eq_d_f (double a, double b, float x, float y)
{
  return (x == y) ? a : b;
}

float
eq_f_f128 (float a, float b, __float128 x, __float128 y)
{
  return (x == y) ? a : b;
}

double
eq_d_f128 (double a, double b, __float128 x, __float128 y)
{
  return (x == y) ? a : b;
}

__float128
eq_f128_f (__float128 a, __float128 b, float x, float y)
{
  return (x == y) ? a : b;
}

__float128
eq_f128_d (__float128 a, __float128 b, double x, double y)
{
  return (x != y) ? a : b;
}

float
ne_f_d (float a, float b, double x, double y)
{
  return (x != y) ? a : b;
}

double
ne_d_f (double a, double b, float x, float y)
{
  return (x != y) ? a : b;
}

float
ne_f_f128 (float a, float b, __float128 x, __float128 y)
{
  return (x != y) ? a : b;
}

double
ne_d_f128 (double a, double b, __float128 x, __float128 y)
{
  return (x != y) ? a : b;
}

__float128
ne_f128_f (__float128 a, __float128 b, float x, float y)
{
  return (x != y) ? a : b;
}

__float128
ne_f128_d (__float128 a, __float128 b, double x, double y)
{
  return (x != y) ? a : b;
}
