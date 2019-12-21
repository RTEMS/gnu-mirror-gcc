/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Test if we generate prefixed loads for vec_extract of a vector float in
   memory, and the memory address has a large offset.  */

#include <altivec.h>

#ifndef TYPE
#define TYPE float
#endif

#ifndef LARGE
#define LARGE 0x50000
#endif

TYPE
get0 (vector TYPE *p)
{
  return vec_extract (p[LARGE], 0);		/* PLFS.  */
}

TYPE
get1 (vector TYPE *p)
{
  return vec_extract (p[LARGE], 1);		/* PLFS.  */
}

/* { dg-final { scan-assembler-times {\mplfs\M}  2 } } */
