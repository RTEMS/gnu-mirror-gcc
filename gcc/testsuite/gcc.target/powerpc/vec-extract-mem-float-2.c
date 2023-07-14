/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

/* Test to verify that the vec_extract with variable element numbers can load
   float elements into a GPR register without doing a LFS/STFS.  */

#include <altivec.h>
#include <stddef.h>

void
extract_v4sf_gpr_n (vector float *p, float *q, size_t n)
{
  float x = vec_extract (*p, n);
  __asm__ (" # %0" : "+r" (x));		/* lwz, no lfs/stfs.  */
  *q = x;
}

/* { dg-final { scan-assembler-times {\mlwzx?\M}               1 } } */
/* { dg-final { scan-assembler-times {\mstw\M}                 1 } } */
/* { dg-final { scan-assembler-not   {\mlfsx?\M|\mlxsspx?\M}     } } */
/* { dg-final { scan-assembler-not   {\mstfsx?\M|\mstxsspx?\M}   } } */
