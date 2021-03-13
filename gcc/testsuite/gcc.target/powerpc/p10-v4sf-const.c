/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test generating VSPLTISW and XXSPLTIW on power10 for V4SF vectors.  */

#include <altivec.h>

vector float
init_zero (void)
{
  return (vector float) { 0.0f, 0.0f, 0.0f, 0.0f };	/* XXSPLTIB  */
}

vector float
init_one (void)
{
  return (vector float) { 1.0f, 1.0f, 1.0f, 1.0f };	/* XXSPLTIW  */
}

vector float
splat_zero (void)
{
  float e = 0.0f;
  return vec_splats (e);				/* XXSPLTIB  */
}

vector float
splat_one (void)
{
  float e = 1.0f;
  return vec_splats (e);				/* XXSPLTIW  */
}

/* { dg-final { scan-assembler-times {\mxxlxor|vspltiw|xxsplitib\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxxspltiw\M}                  2 } } */
/* { dg-final { scan-assembler-not   {\mp?lxvx?\M}                     } } */
