/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

/* Test to verify that the vec_extract with variable element numbers can load
   SImode and convert it to signed floating point, by loading the value
   directly to a vector register, rather than loading up a GPR and transfering
   the result to a vector register.  */

#include <altivec.h>
#include <stddef.h>

double
extract_dbl_sign_v4si_n (vector int *p, size_t n)
{
  return vec_extract (*p, n);	/* lfiwzx/lxsiwzx, fcfid/xscvsxddp.  */
}

float
extract_flt_sign_v4si_element_n_index_4 (vector int *p, size_t n)
{
  return vec_extract (p[4], n);	/* lfiwzx/lxsiwzx, fcfids/xscvsxdsp.  */
}

/* { dg-final { scan-assembler-times {\mlfiwax\M|\mlxsiwax\M}   2 } } */
/* { dg-final { scan-assembler-times {\mfcfid\M|\mxscvsxddp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mfcfids\M|\mxscvsxdsp\M} 1 } } */
/* { dg-final { scan-assembler-not   {\mlw[az]x?\M}               } } */
/* { dg-final { scan-assembler-not   {\mmtvsr}                    } } */
