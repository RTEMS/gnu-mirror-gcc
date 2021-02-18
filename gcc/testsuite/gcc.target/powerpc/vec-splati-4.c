/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#include <altivec.h>

/* Test that XXSPLTIW is generated to load V4SF constants.  */
vector float
splat_0 (void)
{
  return vec_splats (0.0f);	/* vspltisw.  */
}

vector float
splat_3 (void)
{
  return vec_splats (3.0f);	/* xxspltisw.  */
}

/* { dg-final { scan-assembler {\mvspltisw\M|\mxxspltib\M} } } */
/* { dg-final { scan-assembler {\mxxspltiw\M}              } } */
