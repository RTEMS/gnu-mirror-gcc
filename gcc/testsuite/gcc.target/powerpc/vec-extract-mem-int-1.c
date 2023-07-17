/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

/* Test to verify that the vec_extract with constant element numbers can load
   SImode and fold zero extension into the load.  */

#include <altivec.h>

void
extract_uns_v4si_0 (vector unsigned int *p, unsigned int *q)
{
  unsigned int u = vec_extract (*p, 0);
  __asm__ (" # %x0" : "+wa" (u));

  *q = u;
  return;
}

void
extract_uns_v4si_1 (vector unsigned int *p, unsigned int *q)
{
  unsigned int u = vec_extract (*p, 1);
  __asm__ (" # %x0" : "+wa" (u));

  *q = u;
  return;
}

void
extract_uns_v4si_element_0_index_4 (vector unsigned int *p, unsigned int *q)
{
  unsigned int u = vec_extract (p[4], 0);
  __asm__ (" # %x0" : "+wa" (u));

  *q = u;
  return;
}

void
extract_uns_v4si_element_3_index_4 (vector unsigned int *p, unsigned int *q)
{
  unsigned int u = vec_extract (p[4], 0);
  __asm__ (" # %x0" : "+wa" (u));

  *q = u;
  return;
}
