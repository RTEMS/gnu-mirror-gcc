/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* Test to verify that the vec_extract with variable element numbers can load
   float (SF) variables into a GPR without doing a LFS or STFS.  */

#include <altivec.h>
#include <stddef.h>

void
extract_float_0_gpr (vector float *p, float *q, size_t n)
{
  float x = vec_extract (*p, n);
  __asm__ ("# %0" : "+r" (x));			/* lwz.  */
  *q = x;
}

/* { dg-final { scan-assembler-times {\mlwz\M}               1 } } */
/* { dg-final { scan-assembler-times {\mstwz\M}              1 } } */
/* { dg-final { scan-assembler-not   {\mlfs\M|\mlxsspx?\M}     } } */
/* { dg-final { scan-assembler-not   {\mstfs\M|\mstxsspx?\M}   } } */
/* { dg-final { scan-assembler-not   {\mm[tf]vsd}              } } */
/* { dg-final { scan-assembler-not   {\mxscvdpspn?\M}          } } */
