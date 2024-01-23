/* { dg-do compile } */ 
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */ 

#include <altivec.h>

void
foo (__vector_quad *dst, vector unsigned char *ptr, vector unsigned char src)
{
  __vector_quad acc;
  __builtin_mma_xvf32ger(&acc, src, ptr[0]);
  __builtin_mma_xvf32gerpp(&acc, src, ptr[1]);
  *dst = acc;
}
/* { dg-final { scan-assembler {\mlxvp\M} } } */
