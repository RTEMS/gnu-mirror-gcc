/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test generating VSPLTISW and XXSPLTIW on power10 for V4SI vectors.  */

#include <altivec.h>

vector int
init_one (void)
{
  return (vector int) { 1, 1, 1, 1 };	/* VSPLTIW  */
}

vector int
init_127 (void)
{
  return (vector int) { 127, 127, 127, 127 };		/* XXSPLTIW  */
}

vector int
init_500 (void)
{
  return (vector int) { 500, 500, 500, 500 };		/* XXSPLTIW  */
}

vector int
splat_one (void)
{
  int e = 1;
  return vec_splats (e);				/* VSPLTIW  */
}

vector int
splat_127 (void)
{
  int e = 127;
  return vec_splats (e);				/* XXSPLTIW  */
}

vector int
splat_500 (void)
{
  int e = 500;
  return vec_splats (e);				/* XXSPLTIW  */
}

/* { dg-final { scan-assembler-times {\mvsplitsw\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxxspltidp\M} 4 } } */
/* { dg-final { scan-assembler-not   {\mxxspltib\M}    } } */
/* { dg-final { scan-assembler-not   {\mvextsb2w\M}    } } */
/* { dg-final { scan-assembler-not   {\mp?lxvx?\M}     } } */
