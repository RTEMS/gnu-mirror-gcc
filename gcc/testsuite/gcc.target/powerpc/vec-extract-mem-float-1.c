/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

/* Test to verify that the vec_extract with constant element numbers can load
   float (SF) variables into a GPR without doing a LFS or STFS.

   Target LP64 and power8 vector are needed because the compiler only does the
   vec_extract optimizations on 64-bit machines that have direct move support.
   On earlier machines, vec_extract is done by storing the V4SF value into
   memory, and just doing the load from memory.  */

#include <altivec.h>

float
extract_float_0_gpr (vector float *p, float *q)
{
  float x = vec_extract (*p, 0);
  __asm__ ("# %0" : "+r" (x));			/* lwz.  */
  *q = x;
}

float
extract_float_3_gpr (vector float *p, float *q)
{
  float x = vec_extract (*p, 3);
  __asm__ ("# %0" : "+r" (x));			/* lwz.  */
  *q = x;
}

/* { dg-final { scan-assembler-times {\mlwz\M}               2 } } */
/* { dg-final { scan-assembler-times {\mstwz\M}              2 } } */
/* { dg-final { scan-assembler-not   {\mlfs\M|\mlxsspx?\M}     } } */
/* { dg-final { scan-assembler-not   {\mstfs\M|\mstxsspx?\M}   } } */
/* { dg-final { scan-assembler-not   {\mm[tf]vsd}              } } */
/* { dg-final { scan-assembler-not   {\mxscvdpspn?\M}          } } */
