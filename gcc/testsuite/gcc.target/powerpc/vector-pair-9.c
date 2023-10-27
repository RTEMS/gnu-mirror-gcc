/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test vector pair built-in functions to assemble and extract elements.  */

void assemble_two_loads (__vector_pair *p)
{
  vector double a = { 1.0, 2.0 };
  vector double b = { 3.0, 4.0 };

  /* 2 plxv, 1 stxvp.  */
  *p = __builtin_vpair_f64_assemble (a, b);
}

void foo2 (__vector_pair *p)
{
  vector double a = { 1.0, 1.0 };
  vector double b = { 2.0, 2.0 };

  /* 2 xxspltidp, 1 stxvp.  */
  *p = __builtin_vpair_f64_assemble (a, b);
}

void foo3 (__vector_pair *p, __vector_pair *q, __vector_pair *r)
{
  __vector_pair vq = *q;
  __vector_pair vr = *r;
  vector double vq_0 = __builtin_vpair_f64_extract_vector (vq, 0);
  vector double vq_1 = __builtin_vpair_f64_extract_vector (vq, 1);
  vector double vr_0 = __builtin_vpair_f64_extract_vector (vr, 0);
  vector double vr_1 = __builtin_vpair_f64_extract_vector (vr, 1);
  vector double vp_0 = vq_0 + vr_0;
  vector double vp_1 = vq_1 - vr_1;

  /* 2 lxvp, 1 xvadddp, 1 xvsubdp, 1 stxvp.  */
  *p = __builtin_vpair_f64_assemble (vp_0, vp_1);
}

/* { dg-final { scan-assembler-times {\mlxvp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mplxv\M}      2 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}     3 } } */
/* { dg-final { scan-assembler-times {\mxvadddp\M}   1 } } */
/* { dg-final { scan-assembler-times {\mxvsubdp\M}   1 } } */
/* { dg-final { scan-assembler-times {\mxxspltidp\M} 2 } } */
