/* { dg-do compile */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* PR 70423.  Make sure we don't generate vmaddfp or vnmsubfp unless -Ofast is
   used.  These instructions do not round the same way the normal VSX
   instructions do.  These tests are written where the 3 inputs and target are
   all separate registers where the register allocator would prefer to issue
   the 4 argument FMA instruction over the 3 argument instruction plus an extra
   move.  */

#include <altivec.h>

vector float
do_add1 (vector float dummy, vector float a, vector float b, vector float c)
{
  return (a * b) + c;
}

vector float
do_nsub1 (vector float dummy, vector float a, vector float b, vector float c)
{
  return -((a * b) - c);
}

vector float
do_add2 (vector float dummy, vector float a, vector float b, vector float c)
{
  return vec_madd (a, b, c);
}

vector float
do_nsub2 (vector float dummy, vector float a, vector float b, vector float c)
{
  return vec_nmsub (a, b, c);
}

/* { dg-final { scan-assembler     {\mxvmadd[am]sp\M}  } } */
/* { dg-final { scan-assembler     {\mxvnmsub[am]sp\M} } } */
/* { dg-final { scan-assembler-not {\mvmaddfp\M}       } } */
/* { dg-final { scan-assembler-not {\mvnmsubfp\M}      } } */
