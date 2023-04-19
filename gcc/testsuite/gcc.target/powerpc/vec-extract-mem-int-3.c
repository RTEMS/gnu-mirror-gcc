/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target float128_hw } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode and convert the value _Float128 by loading the value directly into a
   vector register, and not loading up the GPRs first.  */

#include <altivec.h>

_Float128
extract_ieee_uns_v4si_0 (vector unsigned int *p)
{
  return vec_extract (*p, 0);		/* lxsiwzx.  */
}

_Float128
extract_ieee_uns_v4si_3 (vector unsigned int *p)
{
  return vec_extract (*p, 3);		/* lxsiwzx.  */
}

/* { dg-final { scan-assembler-times {\mlfiwzx\M|\mlxsiwzx\M}  2 } } */
/* { dg-final { scan-assembler-not   {\mlw[az]\M}                } } */
/* { dg-final { scan-assembler-not   {\mmtvsrw[sz]\M}            } } */
