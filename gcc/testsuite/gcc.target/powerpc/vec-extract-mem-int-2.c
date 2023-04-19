/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode and convert the value to float, and double by loading the value
   directly into a vector register, and not loading up the GPRs first.  */

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

double
extract_double_uns_v4si_3 (vector unsigned int *p)
{
  return vec_extract (*p, 3);		/* lfiwzx or lxsiwzx.  */
}

/* { dg-final { scan-assembler-times {\mlfiwax\M|\mlxsiwax\M}  2 } } */
/* { dg-final { scan-assembler-times {\mlfiwzx\M|\mlxsiwzx\M}  2 } } */
/* { dg-final { scan-assembler-not   {\mlw[az]\M}                } } */
/* { dg-final { scan-assembler-not   {\mmtvsrw[sz]\M}            } } */
