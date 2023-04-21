/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* Test to verify that the vec_extract with variable element numbers can load
   float (SF) variables directly using a single LFSX or LXSSPX instruction.  */

#include <altivec.h>
#include <stddef.h>

float
extract_float_var (vector float *p, size_t n)
{
  return vec_extract (*p, n);		/* lfsx or lxsspx.  */
}

double
extract_float_to_double_var (vector float *p, size_t n)
{
  return vec_extract (*p, n);		/* lfsx or lxsspx.  */
}

/* { dg-final { scan-assembler-times {\mlfsx\M|\{mlxsspx\M}  2 } } */
/* { dg-final { scan-assembler-not   {\mlxv\M|\mlxvx\M}        } } */
/* { dg-final { scan-assembler-not   {\mm[tf]vsr}              } } */
/* { dg-final { scan-assembler-not   {\mvslo\M}                } } */
/* { dg-final { scan-assembler-not   {\mxscvspdp\M}            } } */
