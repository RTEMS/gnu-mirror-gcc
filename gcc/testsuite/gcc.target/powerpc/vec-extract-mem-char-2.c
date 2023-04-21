/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx_ok } */

/* Test to verify that the vec_extract with variable element numbers can load
   QImode and fold the zero extension into the load.  */

#include <altivec.h>
#include <stddef.h>

unsigned long long
extract_uns_var_v16qi (vector unsigned char *p, size_t n)
{
  return vec_extract (*p, n);		/* lbzx, no rlwinm.  */
}

/* { dg-final { scan-assembler-not {\mrlwinm\M} } } */
