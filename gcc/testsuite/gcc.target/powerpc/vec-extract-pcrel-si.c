/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Test if we can support vec_extract on V4SI vectors with a PC-relative
   address.  */

#include <altivec.h>

#ifndef TYPE
#define TYPE unsigned int
#endif

static vector TYPE v;
vector TYPE *p = &v;

TYPE
get0 (void)
{
  return vec_extract (v, 0);
}

TYPE
get1 (void)
{
  return vec_extract (v, 1);
}

TYPE
getn (unsigned long n)
{
  return vec_extract (v, n);
}

/* { dg-final { scan-assembler-times {[@]pcrel}  3 } } */
/* { dg-final { scan-assembler-times {\mplwz\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpla\M}   1 } } */
