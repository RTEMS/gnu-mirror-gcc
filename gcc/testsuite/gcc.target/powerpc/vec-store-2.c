/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* Test that the compiler can optimize creating a vector and immediately
   storing it into doing 2 separate scalar stores.  */

#ifndef TYPE
#define TYPE double
#endif

void foo (vector TYPE *p, TYPE a, TYPE b)
{
  *p = (vector TYPE) { a, b };
}

/* { dg-final { scan-assembler-times {\mstfd\M}     2 } } */
/* { dg-final { scan-assembler-not   {\mvtvsrc\M}     } } */
/* { dg-final { scan-assembler-not   {\mxxpermdi\M}   } } */
/* { dg-final { scan-assembler-not   {\mstxvd2x\M}    } } */
/* { dg-final { scan-assembler-not   {\mstxv\M}       } } */
/* { dg-final { scan-assembler-not   {\mstx\M}        } } */
/* { dg-final { scan-assembler-not   {\mlfd\M}        } } */
