/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx_ok } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode and fold zero extension into the load.  */

#include <altivec.h>

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

unsigned long long
extract_uns_v4si_element_0_index_4 (vector unsigned int *p)
{
  return vec_extract (p[4], 0);		/* lwz, no rldicl.  */
}

unsigned long long
extract_uns_v4si_element_3_index_4 (vector unsigned int *p)
{
  return vec_extract (p[4], 3);		/* lwz, no rldicl.  */
}

/* { dg-final { scan-assembler-times {\mlwz\M}    4 } } */
/* { dg-final { scan-assembler-not   {\mrldicl\M}   } } */
