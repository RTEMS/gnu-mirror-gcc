/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether the vector builtin code generates the expected instructions for
   vector pairs zero and splat functions for vector pairs containing
   floats.  */

void
test_zero (__vector_pair *p)
{
  /* 2 xxspltib/xxlxor.  */
  *p = __builtin_vpair_zero ();
}

void
test_splat_zero (__vector_pair *p)
{
  /* 2 xxspltib/xxlxor.  */
  *p = __builtin_vpair_f32_splat (0.0f);
}

void
test_splat_one (__vector_pair *p)
{
  /* xxspltiw, xxlor.  */
  *p = __builtin_vpair_f32_splat (1.0f);
}

void
test_splat_pi (__vector_pair *p)
{
  /* xxspltiw, xxlor.  */
  *p = __builtin_vpair_f32_splat (3.1415926535f);
}

void
test_splat_arg (__vector_pair *p, float x)
{
  /* xscvdpspn, xxspltw, xxlor.  */
  *p = __builtin_vpair_f32_splat (x);
}

void
test_splat_mem (__vector_pair *p, float *q)
{
  /* xlvwsx, xxlor.  */
  *p = __builtin_vpair_f32_splat (*q);
}

/* { dg-final { scan-assembler-times {\mlxvwsx\M}              1 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}               6 } } */
/* { dg-final { scan-assembler-times {\mxscvdpspn\M}           1 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M|\mxxlxor\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxspltiw\M}            2 } } */
/* { dg-final { scan-assembler-times {\mxxspltw\M}             1 } } */
