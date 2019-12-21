/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Test if we generate prefixed loads for vec_extract of a vector unsigned long
   in memory, and the memory address has a large offset.  */

#include <altivec.h>

#ifndef TYPE
#define TYPE unsigned long
#endif

#ifndef LARGE
#define LARGE 0x50000
#endif

TYPE
get0 (vector TYPE *p)
{
  return vec_extract (p[LARGE], 0);		/* PLD.  */
}

TYPE
get1 (vector TYPE *p)
{
  return vec_extract (p[LARGE], 1);		/* PLD.  */
}

/* { dg-final { scan-assembler-times {\mpld\M}  2 } } */
