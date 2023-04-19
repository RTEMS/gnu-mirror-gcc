/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target vsx } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode and fold the sign/extension into the load.  */

#include <altivec.h>

long long
extract_sign_v8hi_0 (vector short *p)
{
  return vec_extract (*p, 0);          /* lwa, no extsw.  */
}

long long
extract_sign_v8hi_1 (vector short *p)
{
  return vec_extract (*p, 1);          /* lwa, no extsw.  */
}

unsigned long long
extract_uns_v8hi_0 (vector unsigned short *p)
{
  return vec_extract (*p, 0);          /* lwz, no rlwinm.  */
}

unsigned long long
extract_uns_v8hi_1 (vector unsigned short *p)
{
  return vec_extract (*p, 1);          /* lwz, no rlwinm.  */
}

/* { dg-final { scan-assembler-times {\mlha\M}    2 } } */
/* { dg-final { scan-assembler-times {\mlhz\M}    2 } } */
/* { dg-final { scan-assembler-not   {\mextsh\M}    } } */
/* { dg-final { scan-assembler-not   {\mrlwinm\M}   } } */
