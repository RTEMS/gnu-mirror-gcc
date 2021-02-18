/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test that XXSPLTIW is generated to load V4SI constants.  */
#include <altivec.h>

vector int
splat_0 (void)
{
  return vec_splats ((int)0);		/* vspltisw.  */
}

vector int
splat_m1 (void)
{
  return vec_splats ((int)-1);		/* vspltisw.  */
}

vector int
splat_3 (void)
{
  return vec_splats ((int)3);		/* vspltisw.  */
}

vector int
splat_23 (void)
{
  return vec_splats ((int)23);		/* xxspltisw.  */
}

vector int
splat_1023 (void)
{
  return vec_splats ((int)1023);	/* xxspltisw.  */
}

/* Test using xxsplitb to load 0 in FPR registers.  */
void
splat_0_fpr (vector int *p)
{
  vector int v = vec_splats ((int)0);	/* xxspltisb.  */
  __asm__ ("# %x0" : "+f" (v));
  *p = v;
}

/* Test using xxsplitb to load -1 in FPR registers.  */
void
splat_m1_fpr (vector int *p)
{
  vector int v = vec_splats ((int)-1);	/* xxspltisb.  */
  __asm__ ("# %x0" : "+f" (v));
  *p = v;
}

/* { dg-final { scan-assembler-times {\mvspltisw|xxspltib\M} 5 } } */
/* { dg-final { scan-assembler-times {\mxxspltiw\M}          2 } } */
/* { dg-final { scan-assembler-not   {\mvextsb2w\M}            } } */
