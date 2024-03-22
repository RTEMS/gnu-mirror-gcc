/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future2_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future2 -O2" } */

/* Test whether the xvrl (vector word rotate left using VSX registers insead of
   Altivec registers is generated.  */

#include <stddef.h>

size_t
prefix_addis_addi (size_t x)
{
  return x + 0x1234000056789ABCUL;
}

size_t
prefix_addis (size_t x)
{
  return x + 0x123400000000000UL;
}

/* { dg-final { scan-assembler-times {\mpaddis\M} 2  } } */
/* { dg-final { scan-assembler-times {\mpaddi\M}  1  } } */
