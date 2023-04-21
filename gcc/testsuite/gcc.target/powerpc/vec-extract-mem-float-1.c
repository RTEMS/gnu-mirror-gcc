/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* Test to verify that the vec_extract with constant element numbers can load
   float (SF) variables into a GPR without doing a LFS or STFS.  */

#include <altivec.h>

void
extract_float_0_gpr (vector float *p, float *q)
{
  float x = vec_extract (*p, 0);
  __asm__ ("# %0" : "+r" (x));			/* lwz.  */
  *q = x;
}

void
extract_float_1_gpr (vector float *p, float *q)
{
  float x = vec_extract (*p, 1);
  __asm__ ("# %0" : "+r" (x));			/* lwz.  */
  *q = x;
}

/* { dg-final { scan-assembler-times {\mlwz\M}               2 } } */
/* { dg-final { scan-assembler-times {\mstwz\M}              2 } } */
/* { dg-final { scan-assembler-not   {\mlfs\M|\mlxsspx?\M}     } } */
/* { dg-final { scan-assembler-not   {\mstfs\M|\mstxsspx?\M}   } } */
/* { dg-final { scan-assembler-not   {\mm[tf]vsd}              } } */
/* { dg-final { scan-assembler-not   {\mxscvdpspn?\M}          } } */
