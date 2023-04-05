/* { dg-do compile { } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power7" } */

/* PR 70423, Make sure we don't generate fmaddfp or fnmsubfp.  These
   instructions have different rounding modes than the VSX instructions
   xvmaddsp and xvnmsubsp.  */

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
