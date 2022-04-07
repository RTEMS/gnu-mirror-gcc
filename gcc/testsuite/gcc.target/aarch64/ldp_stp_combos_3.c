/* { dg-do assemble } */
/* { dg-options "-O2 -mstrict-align -save-temps" } */

#include <stdint.h>
#include <arm_neon.h>

typedef __int128_t ti;
typedef long double tf;

#define TEST_PAIR(T1, T2)			\
  void						\
  load_fpr_##T1##_##T2 (char *ptr)		\
  {						\
    T1 x1 = *(T1 *) ptr;			\
    T2 x2 = *(T2 *) (ptr + sizeof (T1));	\
    asm volatile ("" :: "w" (x1), "w" (x2));	\
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
  }

#define TEST1(T1)				\
  TEST_PAIR (T1, ti)				\
  TEST_PAIR (T1, tf)				\
  TEST_PAIR (T1, int8x16_t)			\
  TEST_PAIR (T1, int16x8_t)			\
  TEST_PAIR (T1, int32x4_t)			\
  TEST_PAIR (T1, int64x2_t)			\
  TEST_PAIR (T1, float16x8_t)			\
  TEST_PAIR (T1, float32x4_t)			\
  TEST_PAIR (T1, float64x2_t)

TEST1 (ti)
TEST1 (tf)
TEST1 (int8x16_t)
TEST1 (int16x8_t)
TEST1 (int32x4_t)
TEST1 (int64x2_t)
TEST1 (float16x8_t)
TEST1 (float32x4_t)
TEST1 (float64x2_t)

/* { dg-final { scan-assembler-times {\tldp\tq[0-9]+,} 81 } } */
/* { dg-final { scan-assembler-times {\tstp\tq[0-9]+,} 81 } } */
