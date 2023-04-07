/* { dg-do compile */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* PR 70423, Make sure we don't generate fmaddfp or fnmsubfp.  These
   instructions have different rounding modes than the VSX instructions
   xvmaddsp and xvnmsubsp.  These tests are written where the 3 inputs and
   target are all separate registers.  Because fmaddfp and fnmsubfp are no
   longer generated the compiler will have to generate an xsmaddsp or xsnmsubsp
   instruction followed by a move operation.  */

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

/* { dg-final { scan-assembler     "xvmaddsp"  } } */
/* { dg-final { scan-assembler     "xvnmsubsp" } } */
/* { dg-final { scan-assembler-not "fmaddfp"   } } */
/* { dg-final { scan-assembler-not "fnmsubfp"  } } */
