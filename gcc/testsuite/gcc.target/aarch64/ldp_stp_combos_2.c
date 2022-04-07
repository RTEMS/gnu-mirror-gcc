/* { dg-do assemble } */
/* { dg-options "-O2 -mstrict-align -save-temps" } */

#include <stdint.h>
#include <arm_neon.h>

#define TEST_PAIR(T1, T2)			\
  void						\
  load_gpr_##T1##_##T2 (char *ptr)		\
  {						\
    T1 x1 = *(T1 *) ptr;			\
    T2 x2 = *(T2 *) (ptr + sizeof (T1));	\
    asm volatile ("" :: "r" (x1), "r" (x2));	\
  }						\
						\
  void						\
  load_fpr_##T1##_##T2 (char *ptr)		\
  {						\
    T1 x1 = *(T1 *) ptr;			\
    T2 x2 = *(T2 *) (ptr + sizeof (T1));	\
    asm volatile ("" :: "w" (x1), "w" (x2));	\
  }						\
						\
  void						\
  store_gpr_##T1##_##T2 (char *ptr)		\
  {						\
    T1 x1;					\
    T2 x2;					\
    asm volatile ("" : "=r" (x1), "=r" (x2));	\
    *(T1 *) ptr = x1;				\
    *(T2 *) (ptr + sizeof (T1)) = x2;		\
  }						\
						\
  void						\
  store_fpr_##T1##_##T2 (char *ptr)		\
  {						\
    T1 x1;					\
    T2 x2;					\
    asm volatile ("" : "=w" (x1), "=w" (x2));	\
    *(T1 *) ptr = x1;				\
    *(T2 *) (ptr + sizeof (T1)) = x2;		\
  }						\
						\
  void						\
  store_zero_##T1##_##T2 (char *ptr)		\
  {						\
    *(T1 *) ptr = (T1) { 0 };			\
    *(T2 *) (ptr + sizeof (T1)) = (T2) { 0 };	\
  }

#define TEST1(T1)				\
  TEST_PAIR (T1, int64_t)			\
  TEST_PAIR (T1, double)			\
  TEST_PAIR (T1, int8x8_t)			\
  TEST_PAIR (T1, int16x4_t)			\
  TEST_PAIR (T1, int32x2_t)			\
  TEST_PAIR (T1, int64x1_t)			\
  TEST_PAIR (T1, float16x4_t)			\
  TEST_PAIR (T1, float32x2_t)			\
  TEST_PAIR (T1, float64x1_t)

TEST1 (int64_t)
TEST1 (double)
TEST1 (int8x8_t)
TEST1 (int16x4_t)
TEST1 (int32x2_t)
TEST1 (int64x1_t)
TEST1 (float16x4_t)
TEST1 (float32x2_t)
TEST1 (float64x1_t)

/* { dg-final { scan-assembler-times {\tldp\tx[0-9]+,} 81 } } */
/* { dg-final { scan-assembler-times {\tldp\td[0-9]+,} 81 } } */
/* { dg-final { scan-assembler-times {\tstp\tx[0-9]+,} 81 } } */
/* { dg-final { scan-assembler-times {\tstp\td[0-9]+,} 81 } } */
/* { dg-final { scan-assembler-times {\tstp\txzr,} 81 } } */
