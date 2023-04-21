/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

/* Test to verify that the vec_extract with variable element numbers can load
   SImode and convert the value to float, and double by loading the value
   directly into a vector register, and not loading up the GPRs first.  */

#include <altivec.h>
#include <stddef.h>

float
extract_float_sign_v4si_var (vector int *p, size_t n)
{
  return vec_extract (*p, n);		/* lfiwax or lxsiwax.  */
}

double
extract_double_uns_v4si_var (vector unsigned int *p, size_t n)
{
  return vec_extract (*p, n);		/* lfiwzx or lxsiwzx.  */
}

/* { dg-final { scan-assembler-times {\mlfiwax\M|\mlxsiwax\M}  1 } } */
/* { dg-final { scan-assembler-times {\mlfiwzx\M|\mlxsiwzx\M}  1 } } */
/* { dg-final { scan-assembler-not   {\mlw[az]\M}                } } */
/* { dg-final { scan-assembler-not   {\mmtvsrw[sz]\M}            } } */
