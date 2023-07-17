/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode and convert it to signed floating point, by loading the value
   directly to a vector register, rather than loading up a GPR and transfering
   the result to a vector register.  */

#include <altivec.h>

double
extract_dbl_sign_v4si_0 (vector int *p)
{
  return vec_extract (*p, 0);	/* lfiwzx/lxsiwzx, fcfid/xscvsxddp.  */
}

double
extract_dbl_sign_v4si_1 (vector int *p)
{
  return vec_extract (*p, 1);	/* lfiwzx/lxsiwzx, fcfid/xscvsxddp.  */
}

float
extract_flt_sign_v4si_element_0_index_4 (vector int *p)
{
  return vec_extract (p[4], 0);	/* lfiwzx/lxsiwzx, fcfids/xscvsxdsp.  */
}

float
extract_flt_sign_v4si_element_3_index_4 (vector int *p)
{
  return vec_extract (p[4], 3);	/* lfiwzx/lxsiwzx, fcfids/xscvsxdsp.  */
}

/* { dg-final { scan-assembler-times {\mlfiwax\M|\mlxsiwax\M}   4 } } */
/* { dg-final { scan-assembler-times {\mfcfid\M|\mxscvsxddp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mfcfids\M|\mxscvsxdsp\M} 2 } } */
/* { dg-final { scan-assembler-not   {\mlw[az]x?\M}               } } */
/* { dg-final { scan-assembler-not   {\mmtvsr}                    } } */
