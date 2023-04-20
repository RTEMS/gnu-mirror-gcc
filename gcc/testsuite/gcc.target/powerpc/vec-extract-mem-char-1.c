/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx_ok } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode and fold the sign/extension into the load.  */

#include <altivec.h>

unsigned long long
extract_uns_v16qi_0 (vector unsigned char *p)
{
  return vec_extract (*p, 0);          /* lbz, no rlwinm.  */
}

unsigned long long
extract_uns_v16qi_1 (vector unsigned char *p)
{
  return vec_extract (*p, 1);          /* lbz, no rlwinm.  */
}

/* { dg-final { scan-assembler-not {\mrlwinm\M} } } */
