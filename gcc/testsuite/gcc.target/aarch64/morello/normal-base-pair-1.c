/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } { "" } }  */

#include <arm_neon.h>

void
fpr (uint32x4_t *res, uint32x2_t v0, uint32x2_t v1)
{
  *res = vcombine_u32 (v0, v1);
}

void
gpr (uint32x4_t *res)
{
  uint32x2_t v0, v1;
  asm ("" : "=r" (v0), "=r" (v1));
  *res = vcombine_u32 (v0, v1);
}

/* { dg-final { scan-assembler-times {\tstp\t} 2 } } */
