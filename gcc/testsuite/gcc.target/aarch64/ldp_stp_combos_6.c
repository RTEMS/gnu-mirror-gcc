/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

#pragma GCC target "+bf16"

#include <stdint.h>
#include <arm_neon.h>

void
load1 (char *ptr)
{
  int64_t x1 = *(int64_t *)(ptr + 1);
  double x2 = *(double *)(ptr + 9);
  int8x8_t x3 = *(int8x8_t *)(ptr + 17);
  int16x4_t x4 = *(int16x4_t *)(ptr + 25);
  asm volatile ("" :: "r" (x1), "r" (x2), "r" (x3), "r" (x4));
}

void
load2 (char *ptr)
{
  int32x2_t x1 = *(int32x2_t *)(ptr + 1);
  int64x1_t x2 = *(int64x1_t *)(ptr + 9);
  float16x4_t x3 = *(float16x4_t *)(ptr + 17);
  float32x2_t x4 = *(float32x2_t *)(ptr + 25);
  asm volatile ("" :: "w" (x1), "w" (x2), "w" (x3), "w" (x4));
}

void
store1 (char *ptr)
{
  float64x1_t x1;
  bfloat16x4_t x2;
  int64_t x3;
  double x4;
  asm ("" : "=r" (x1), "=r" (x2), "=r" (x3), "=r" (x4));
  *(typeof(x1) *)(ptr + 1) = x1;
  *(typeof(x2) *)(ptr + 9) = x2;
  *(typeof(x3) *)(ptr + 17) = x3;
  *(typeof(x4) *)(ptr + 25) = x4;
}

void
store2 (char *ptr)
{
  int64_t x1;
  float32x2_t x2;
  int32x2_t x3;
  int16x4_t x4;
  asm ("" : "=w" (x1), "=w" (x2), "=w" (x3), "=w" (x4));
  *(typeof(x1) *)(ptr + 1) = x1;
  *(typeof(x2) *)(ptr + 9) = x2;
  *(typeof(x3) *)(ptr + 17) = x3;
  *(typeof(x4) *)(ptr + 25) = x4;
}

void
store3 (char *ptr)
{
  *(int64_t *)(ptr + 1) = 0;
  *(double *)(ptr + 9) = 0;
  *(int32x2_t *)(ptr + 17) = (int32x2_t) { 0, 0 };
  *(float32x2_t *)(ptr + 25) = (float32x2_t) { 0, 0 };
}

/* { dg-final { scan-assembler-times {\tldp\tx[0-9]+,} 2 } } */
/* { dg-final { scan-assembler-times {\tldp\td[0-9]+,} 2 } } */
/* { dg-final { scan-assembler-times {\tstp\tx[0-9]+,} 2 } } */
/* { dg-final { scan-assembler-times {\tstp\td[0-9]+,} 2 } } */
/* { dg-final { scan-assembler-times {\tstp\txzr,} 2 } } */
