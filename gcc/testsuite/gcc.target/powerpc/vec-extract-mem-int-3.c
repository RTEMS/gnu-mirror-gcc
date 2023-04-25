/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx_ok } */

/* Test to verify that the vec_extract with variable element numbers can load
   SImode and fold both zero and sign extension into the load.  */

#include <altivec.h>
#include <stddef.h>

long long
extract_sign_v8hi_var (vector int *p, size_t n)
{
  return vec_extract (*p, n);		/* lwax, no extsw.  */
}

unsigned long long
extract_uns_v8hi_var (vector unsigned int *p, size_t n)
{
  return vec_extract (*p, n);		/* lwzx, no rldicl.  */
}

/* { dg-final { scan-assembler     {\mlwax\M}   } } */
/* { dg-final { scan-assembler     {\mlwzx\M}   } } */
/* { dg-final { scan-assembler-not {\mlw[az]\M} } } */
/* { dg-final { scan-assembler-not {\mextsw\M}  } } */
/* { dg-final { scan-assembler-not {\mrldicl\M} } } */
