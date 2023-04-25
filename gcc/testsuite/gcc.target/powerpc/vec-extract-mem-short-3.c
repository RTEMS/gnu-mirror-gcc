/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx_ok } */

/* Test to verify that the vec_extract with variable element numbers can load
   HImode and fold sign extension into the load.  */

#include <altivec.h>
#include <stddef.h>

long long
extract_sign_v8hi_var (vector short *p, size_t n)
{
  return vec_extract (*p, n);		/* lwax, no extsw.  */
}

/* { dg-final { scan-assembler     {\mlhax\M}   } } */
/* { dg-final { scan-assembler-not {\mlh[az]\M} } } */
/* { dg-final { scan-assembler-not {\mextsh\M}  } } */
