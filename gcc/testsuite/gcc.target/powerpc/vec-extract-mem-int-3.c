/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode and convert the value to float, double, and _Float128 by loading the
   value directly into a vector register, and not loading up the GPRs
   first.  */

#include <altivec.h>

float
extract_float_sign_v4si_0 (vector int *p)
{
  return vec_extract (*p, 0);		/* lfiwax or lxsiwax.  */
}

double
extract_double_sign_v4si_1 (vector int *p)
{
  return vec_extract (*p, 1);		/* lfiwax or lxsiwax.  */
}

double
extract_double_uns_v4si_0 (vector unsigned int *p)
{
  return vec_extract (*p, 0);		/* lfiwzx or lxsiwzx.  */
}

_Float128
extract_ieee_uns_v4si_1 (vector unsigned int *p)
{
  return vec_extract (*p, 1);		/* lfiwzx or lxsiwzx.  */
}

/* { dg-final { scan-assembler-times {\mlfiwax\M|\mlxsiwax\M}  2 } } */
/* { dg-final { scan-assembler-times {\mlfiwzx\M|\mlxsiwzx\M}  2 } } */
/* { dg-final { scan-assembler-not   {\mlw[az]\M}                } } */
/* { dg-final { scan-assembler-not   {\mmtvsrw[sz]\M}            } } */
