/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test generating XXSPLTIDP on power10 for V2DF vectors.  */

#include <altivec.h>

vector double
init_zero (void)
{
  return (vector double) { 0.0, 0.0 };			/* XXSPLTIB  */
}

vector double
init_one (void)
{
  return (vector double) { 1.0, 1.0 };			/* XXSPLTIDP  */
}

vector double
splat_zero (void)
{
  return vec_splats (0.0);				/* XXSPLTIB  */
}

vector double
splat_one (void)
{
  return vec_splats (1.0);				/* XXSPLTIDP  */
}

/* { dg-final { scan-assembler-times {\mxxlxor|vspltiw|xxspltib\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxxspltidp\M}                2 } } */
/* { dg-final { scan-assembler-not   {\mp?lxvx?\M}                    } } */
