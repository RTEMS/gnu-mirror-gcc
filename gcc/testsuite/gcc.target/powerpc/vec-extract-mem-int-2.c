/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx_ok } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode and fold sign extension into the load.  */

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

long long
extract_sign_v4si_element_0_index_4 (vector int *p)
{
  return vec_extract (p[4], 0);		/* lwa, no extsw.  */
}

long long
extract_sign_v4si_element_3_index_4 (vector int *p)
{
  return vec_extract (p[4], 3);		/* lwa, no extsw.  */
}

/* { dg-final { scan-assembler-times {\mlwa\M}   4 } } */
/* { dg-final { scan-assembler-not   {\mlwz\M}     } } */
/* { dg-final { scan-assembler-not   {\mextsw\M}   } } */

extract_sign_v4si_0 (vector int *p)
{
  return vec_extract (*p, 0);          /* lwa, no extsw.  */
}

long long
extract_sign_v4si_1 (vector int *p)
{
  return vec_extract (*p, 1);          /* lwa, no extsw.  */
}

long long
extract_sign_v4si_element_0_index_4 (vector int *p)
{
  return vec_extract (p[4], 0);		/* lwa, no extsw.  */
}

long long
extract_sign_v4si_element_3_index_4 (vector int *p)
{
  return vec_extract (p[4], 3);		/* lwa, no extsw.  */
}

/* { dg-final { scan-assembler-times {\mlwa\M}   4 } } */
/* { dg-final { scan-assembler-not   {\mlwz\M}     } } */
/* { dg-final { scan-assembler-not   {\mextsw\M}   } } */
