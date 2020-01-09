/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Test if we can support vec_extract on V4SF vectors with a large numeric
   offset address.  */

#include <altivec.h>

#ifndef TYPE
#define TYPE float
#endif

#ifndef OFFSET
#define OFFSET 0x12345
#endif

TYPE
get0 (vector TYPE *p)
{
  return vec_extract (p[OFFSET], 0);
}

TYPE
get1 (vector TYPE *p)
{
  return vec_extract (p[OFFSET], 1);
}

TYPE
getn (vector TYPE *p, unsigned long n)
{
  return vec_extract (p[OFFSET], n);
}

/* { dg-final { scan-assembler-times {\mplfs\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpaddi\M} 1 } } */
