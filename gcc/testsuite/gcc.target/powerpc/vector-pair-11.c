/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test the vector pair built-in functions for creation and extraction of
   vector pair operations using 64-bit doubles.  */

void
test_f64_splat_0 (__vector_pair *p)
{
  /* 2 xxspltib.  */
  *p = __builtin_vpair_f64_splat (0.0);
}

void
test_f64_splat_1 (__vector_pair *p)
{
  /* 1 xxspltidp, 1 xxlor.  */
  *p = __builtin_vpair_f64_splat (1.0);
}

void
test_f64_splat_var (__vector_pair *p,
		    double d)
{
  /* 1 xxpermdi, 1 xxlor.  */
  *p = __builtin_vpair_f64_splat (d);
}

void
test_f64_splat_mem (__vector_pair *p,
		    double *q)
{
  /* 1 lxvdsx, 1 xxlor.  */
  *p = __builtin_vpair_f64_splat (*q);
}

void
test_f64_assemble (__vector_pair *p,
		   vector double v1,
		   vector double v2)
{
  /* 2 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_f64_assemble (v1, v2);
}

vector double
test_f64_extract_0_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_f64_extract_vector (vp, 0);
}

vector double
test_f64_extract_1_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_f64_extract_vector (vp, 0);
}

vector double
test_f64_extract_0_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_f64_extract_vector (p[1], 0);
}

vector double
test_f64_extract_1_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_f64_extract_vector (p[2], 1);
}

/* { dg-final { scan-assembler-times {\mlxvdsx\M}    1 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}     5 } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M}  1 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxxspltidp\M} 1 } } */
