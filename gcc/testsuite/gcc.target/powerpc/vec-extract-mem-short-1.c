/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode and fold zero extension into the load.  */

#include <altivec.h>

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

unsigned long long
extract_uns_v8hi_element_0_index_4 (vector unsigned short *p)
{
  return vec_extract (p[4], 0);		/* lbz, no rlwinm.  */
}

unsigned long long
extract_uns_v8hi_element_3_index_4 (vector unsigned short *p)
{
  return vec_extract (p[4], 3);		/* lbz, no rlwinm.  */
}

/* { dg-final { scan-assembler-times {\mlhzx?\M}  4 } } */
/* { dg-final { scan-assembler-not   {\mrlwinm\M}   } } */
