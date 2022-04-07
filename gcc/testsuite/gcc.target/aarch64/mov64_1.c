/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdint.h>
#include <arm_neon.h>

#define TEST(T)					\
  void						\
  load_gpr_##T (T *ptr)				\
  {						\
    asm volatile ("" :: "r" (*ptr));		\
  }						\
						\
  void						\
  load_gpr_##T##_lsl3 (T *ptr, intptr_t i)	\
  {						\
    asm volatile ("" :: "r" (ptr[i]));		\
  }						\
						\
  void						\
  load_gpr_##T##_m256 (T *ptr)			\
  {						\
    asm volatile ("" :: "r" (ptr[-32]));	\
  }						\
						\
  void						\
  load_gpr_##T##_32760 (T *ptr)			\
  {						\
    asm volatile ("" :: "r" (ptr[4095]));	\
  }						\
						\
  void						\
  store_gpr_##T (T *ptr)			\
  {						\
    asm volatile ("" : "=r" (*ptr));		\
  }						\
						\
  void						\
  store_zero_##T (T *ptr)			\
  {						\
    *ptr = (T) { 0 };				\
  }

TEST (int64_t)
TEST (double)
TEST (int8x8_t)
TEST (int16x4_t)
TEST (int32x2_t)
TEST (int64x1_t)
TEST (float16x4_t)
TEST (float32x2_t)
TEST (float64x1_t)

/* { dg-final { scan-assembler-times {\tldr\tx[0-9]+, \[[cx]0\]\n} 9 } } */
/* { dg-final { scan-assembler-times {\tldr\tx[0-9]+, \[[cx]0, #?-256\]\n} 9 } } */
/* { dg-final { scan-assembler-times {\tldr\tx[0-9]+, \[[cx]0, #?32760\]\n} 9 } } */
/* { dg-final { scan-assembler-times {\tldr\tx[0-9]+, \[[cx]0, x1, lsl #?3\]\n} 9 } } */
/* { dg-final { scan-assembler-times {\tstr\tx[0-9]+, \[[cx]0\]\n} 9 } } */
/* { dg-final { scan-assembler-times {\tstr\txzr, \[[cx]0\]\n} 9 } } */
