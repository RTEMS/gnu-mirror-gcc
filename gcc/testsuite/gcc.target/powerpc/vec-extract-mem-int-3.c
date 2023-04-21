/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx_ok } */

/* Test to verify that the vec_extract with variable element numbers can load
   SImode and fold the sign/extension into the load.  */

#include <altivec.h>
#include <stddef.h>

long long
extract_sign_v4si_var (vector int *p, size_t n)
{
  return vec_extract (*p, n);		/* lwax, no extsw.  */
}

unsigned long long
extract_uns_v4si_var (vector unsigned int *p, size_t n)
{
  return vec_extract (*p, n);		/* lwzx, no rldicl.  */
}

/* { dg-final { scan-assembler-times {\mlwax\M}   1 } } */
/* { dg-final { scan-assembler-times {\mlwzx\M}   1 } } */
/* { dg-final { scan-assembler-times {\mrldicl\M} 2 } } */
/* { dg-final { scan-assembler-not   {\mextsw\M}    } } */
