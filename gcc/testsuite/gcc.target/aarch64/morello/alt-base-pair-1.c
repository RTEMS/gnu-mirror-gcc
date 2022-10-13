/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <arm_neon.h>

void
fpr (uint32x4_t *__capability res, uint32x2_t v0, uint32x2_t v1)
{
  *res = vcombine_u32 (v0, v1);
}

void
gpr (uint32x4_t *__capability res)
{
  uint32x2_t v0, v1;
  asm ("" : "=r" (v0), "=r" (v1));
  *res = vcombine_u32 (v0, v1);
}

/* { dg-final { scan-assembler-not {\tstp\t} } } */
