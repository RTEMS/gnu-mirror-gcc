/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */
/* { dg-require-effective-target p8vector_hw } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode directly into vector registers.  */

#include <altivec.h>

void
extract_sign_v4si_0 (vector int *p, int *q)
{
  int x = vec_extract (*p, 0);
  __asm__ (" # %x0" : "+wa" (x));	/* lfiwzx or lfiwax.  */
  *q = x;
}

void
extract_sign_v4si_1 (vector int *p, int *q)
{
  int x = vec_extract (*p, 1);
  __asm__ (" # %x0" : "+wa" (x));	/* lfiwzx or lfiwax.  */
  *q = x;
}

void
extract_uns_v4si_0 (vector unsigned int *p, unsigned int *q)
{
  int x = vec_extract (*p, 0);
  __asm__ (" # %x0" : "+wa" (x));	/* lfiwzx or lfiwax.  */
  *q = x;
}

void
extract_v4si_1 (vector unsigned int *p, unsigned int *q)
{
  int x = vec_extract (*p, 1);
  __asm__ (" # %x0" : "+wa" (x));	/* lfiwzx or lfiwax.  */
  *q = x;
}

/* { dg-final { scan-assembler-times {\mlfiw[az]x\M} 4 } } */
/* { dg-final { scan-assembler-not   {\mlw[az]x\M}     } } */
