/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */
/* { dg-require-effective-target p8vector_hw } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode and fold the sign/extension into the load.  */

#include <altivec.h>

long long
extract_sign_v8hi_0 (vector short *p)
{
  return vec_extract (*p, 0);		/* lha, no extsh.  */
}

long long
extract_sign_v8hi_1 (vector short *p)
{
  return vec_extract (*p, 1);		/* lha, no extsh.  */
}

unsigned long long
extract_uns_v8hi_0 (vector unsigned short *p)
{
  return vec_extract (*p, 0);		/* lhz, no rlwinm.  */
}

unsigned long long
extract_uns_v8hi_1 (vector unsigned short *p)
{
  return vec_extract (*p, 1);		/* lhz, no rlwinm.  */
}

/* { dg-final { scan-assembler-times {\mlha\M}   2 } } */
/* { dg-final { scan-assembler-times {\mlhz\M}   2 } } */
/* { dg-final { scan-assembler-not   {\mextsh\M}   } } */
/* { dg-final { scan-assembler-not   {\mrlwinm\M}  } } */
