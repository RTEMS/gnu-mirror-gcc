/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test the vector pair built-in functions for creation and extraction of
   vector pair operations using 32-bit floats.  */

void
test_f32_splat_0 (__vector_pair *p)
{
  /* 2 xxspltib, 1 stxvp.  */
  *p = __builtin_vpair_f32_splat (0.0f);
}

void
test_f32_splat_1 (__vector_pair *p)
{
  /* 1 xxspltiw, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_f32_splat (1.0f);
}

void
test_f32_splat_var (__vector_pair *p,
		    float f)
{
  /* 1 xscvdpspn, 1 xxspltw, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_f32_splat (f);
}

void
test_f32_splat_mem (__vector_pair *p,
		    float *q)
{
  /* 1 lxvwsx, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_f32_splat (*q);
}

void
test_f32_assemble (__vector_pair *p,
		   vector float v1,
		   vector float v2)
{
  /* 2 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_f32_assemble (v1, v2);
}

vector float
test_f32_extract_0_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_f32_extract_vector (vp, 0);
}

vector float
test_f32_extract_1_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_f32_extract_vector (vp, 0);
}

vector float
test_f32_extract_0_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_f32_extract_vector (p[1], 0);
}

vector float
test_f32_extract_1_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_f32_extract_vector (p[2], 1);
}

/* { dg-final { scan-assembler-times {\mlxv\M}       2 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mlxvwsx\M}    1 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}     5 } } */
/* { dg-final { scan-assembler-times {\mxscvdpspn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxxspltiw\M}  1 } } */
/* { dg-final { scan-assembler-times {\mxxspltw\M}   1 } } */
