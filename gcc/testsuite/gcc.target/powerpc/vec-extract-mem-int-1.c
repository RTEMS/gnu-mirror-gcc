/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target vsx } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode and fold the sign/extension into the load.  */

#include <altivec.h>

long long
extract_sign_v4si_0 (vector int *p)
{
  return vec_extract (*p, 0);          /* lwa, no extsw.  */
}

long long
extract_sign_v4si_1 (vector int *p)
{
  return vec_extract (*p, 1);          /* lwa, no extsw.  */
}

unsigned long long
extract_uns_v4si_0 (vector unsigned int *p)
{
  return vec_extract (*p, 0);          /* lwz, no rldicl.  */
}

unsigned long long
extract_uns_v4si_1 (vector unsigned int *p)
{
  return vec_extract (*p, 1);          /* lwz, no rldicl.  */
}

/* { dg-final { scan-assembler-times {\mlwa\M}   2 } } */
/* { dg-final { scan-assembler-times {\mlwz\M}   2 } } */
/* { dg-final { scan-assembler-not   {\mextsw\M}   } } */
/* { dg-final { scan-assembler-not   {\mrldicl\M}  } } */
