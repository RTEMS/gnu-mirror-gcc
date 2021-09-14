/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test generating V2DImode constants that have the same bit pattern as
   V2DFmode constants that can be loaded with the XXSPLTIDP instruction with
   the ISA 3.1 (power10).  */

vector long long
vector_0 (void)
{
  /* XXSPLTIB or XXLXOR.  */
  return (vector long long) { 0LL, 0LL };
}

vector long long
vector_1 (void)
{
  /* XXSPLTIB and VEXTSB2D.  */
  return (vector long long) { 1LL, 1LL };
}

vector long long
vector_neg_0 (void)
{
  /* XXSPLTIDP.  */
  return (vector long long) { 0x80000000LL, 0x80000000LL };
}

vector long long
scalar_large_value (void)
{
  /* PLXV.  */
  return (vector long long) { 0x80000001LL, 0x80000001LL };
}

/* { dg-final { scan-assembler-times {\mxxspltidp\M} 1 } } */
