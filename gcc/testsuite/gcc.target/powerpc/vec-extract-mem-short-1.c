/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* Test to verify that the vec_extract with constant element numbers can load
   HImode into a vector register and store it without using a direct move
   operation.  */

#include <altivec.h>

void
extract_uns_v8hi_0 (vector unsigned short *p, unsigned short *q)
{
  unsigned short u = vec_extract (*p, 0);
  __asm__ (" # %x0" : "+wa" (u));

  *q = u;
  return;
}

void
extract_uns_v8hi_1 (vector unsigned short *p, unsigned short *q)
{
  unsigned short u = vec_extract (*p, 1);
  __asm__ (" # %x0" : "+wa" (u));

  *q = u;
  return;
}

void
extract_uns_v8hi_element_0_index_4 (vector unsigned short *p,
				    unsigned short *q)
{
  unsigned short u = vec_extract (p[4], 0);
  __asm__ (" # %x0" : "+wa" (u));

  *q = u;
  return;
}

void
extract_uns_v8hi_element_3_index_4 (vector unsigned short *p,
				    unsigned short *q)
{
  unsigned short u = vec_extract (p[4], 0);
  __asm__ (" # %x0" : "+wa" (u));

  *q = u;
  return;
}

/* { dg-final { scan-assembler-times {\mlxsihzx\M}  4 } } */
/* { dg-final { scan-assembler-not   {\mlhzx?\M}      } } */
/* { dg-final { scan-assembler-not   {\mmtvsrwz\M}    } } */
