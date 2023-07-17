/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* Test to verify that the vec_extract with constant element numbers can load
   QImode into a vector register and store it without using a direct move
   operation.  */

#include <altivec.h>

void
extract_uns_v16qi_0 (vector unsigned char *p, unsigned char *q)
{
  unsigned char u = vec_extract (*p, 0);
  __asm__ (" # %x0" : "+wa" (u));

  *q = u;
  return;
}

void
extract_uns_v16qi_1 (vector unsigned char *p, unsigned char *q)
{
  unsigned char u = vec_extract (*p, 1);
  __asm__ (" # %x0" : "+wa" (u));

  *q = u;
  return;
}

void
extract_uns_v16qi_element_0_index_4 (vector unsigned char *p,
				    unsigned char *q)
{
  unsigned char u = vec_extract (p[4], 0);
  __asm__ (" # %x0" : "+wa" (u));

  *q = u;
  return;
}

void
extract_uns_v16qi_element_3_index_4 (vector unsigned char *p,
				    unsigned char *q)
{
  unsigned char u = vec_extract (p[4], 0);
  __asm__ (" # %x0" : "+wa" (u));

  *q = u;
  return;
}

/* { dg-final { scan-assembler-times {\mlxsibzx\M}  4 } } */
/* { dg-final { scan-assembler-not   {\mlbzx?\M}      } } */
/* { dg-final { scan-assembler-not   {\mmtvsrwz\M}    } } */
