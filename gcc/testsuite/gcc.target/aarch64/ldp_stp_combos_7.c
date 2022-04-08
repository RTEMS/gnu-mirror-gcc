/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

#pragma GCC target "+bf16"

#include <stdint.h>
#include <arm_neon.h>

void
load1 (char *ptr)
{
  int8x16_t x1 = *(int8x16_t *)(ptr + 1);
  int16x8_t x2 = *(int16x8_t *)(ptr + 17);
  int32x4_t x3 = *(int32x4_t *)(ptr + 33);
  int64x2_t x4 = *(int64x2_t *)(ptr + 49);
  asm volatile ("" :: "w" (x1), "w" (x2), "w" (x3), "w" (x4));
}

void
load2 (char *ptr)
{
  bfloat16x8_t x1 = *(bfloat16x8_t *)(ptr + 1);
  float16x8_t x2 = *(float16x8_t *)(ptr + 17);
  float32x4_t x3 = *(float32x4_t *)(ptr + 33);
  float64x2_t x4 = *(float64x2_t *)(ptr + 49);
  asm volatile ("" :: "w" (x1), "w" (x2), "w" (x3), "w" (x4));
}

void
store1 (char *ptr)
{
  float64x2_t x1;
  float32x4_t x2;
  float16x8_t x3;
  bfloat16x8_t x4;
  asm ("" : "=w" (x1), "=w" (x2), "=w" (x3), "=w" (x4));
  *(typeof(x1) *)(ptr + 1) = x1;
  *(typeof(x2) *)(ptr + 17) = x2;
  *(typeof(x3) *)(ptr + 33) = x3;
  *(typeof(x4) *)(ptr + 49) = x4;
}

void
store2 (char *ptr)
{
  int64x2_t x1;
  int32x4_t x2;
  int16x8_t x3;
  int8x16_t x4;
  asm ("" : "=w" (x1), "=w" (x2), "=w" (x3), "=w" (x4));
  *(typeof(x1) *)(ptr + 1) = x1;
  *(typeof(x2) *)(ptr + 17) = x2;
  *(typeof(x3) *)(ptr + 33) = x3;
  *(typeof(x4) *)(ptr + 49) = x4;
}

/* { dg-final { scan-assembler-times {\tldp\tq[0-9]+,} 4 } } */
/* { dg-final { scan-assembler-times {\tstp\tq[0-9]+,} 4 } } */
