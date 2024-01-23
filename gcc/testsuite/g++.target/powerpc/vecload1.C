/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#include <altivec.h>
	
void
foo2 ()
{
  __vector_quad *dst1;
  __vector_quad *dst2;
  vector unsigned char src;
  __vector_quad acc;
  vector unsigned char *ptr;
  __builtin_mma_xvf32ger(&acc, src, ptr[0]);
  __builtin_mma_xvf32gerpp(&acc, src, ptr[1]);
  *dst1 = acc;
  __builtin_mma_xvf32ger(&acc, src, ptr[2]);
  __builtin_mma_xvf32gerpp(&acc, src, ptr[3]);
  *dst2 = acc;
}
/* { dg-final { scan-assembler {\mlxvp\M} } } */
