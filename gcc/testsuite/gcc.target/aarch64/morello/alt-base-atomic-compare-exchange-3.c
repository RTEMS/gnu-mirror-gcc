/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_SIZE(TYPE, SIZE)						\
  TYPE									\
  test_##TYPE##_relaxed (TYPE *__capability ptr, TYPE *expected,	\
			 TYPE desired)					\
  {									\
    return __atomic_compare_exchange_n (ptr, expected, desired,		\
					0, __ATOMIC_RELAXED,		\
					__ATOMIC_RELAXED);		\
  }									\
									\
  TYPE									\
  test_##TYPE##_acquire (TYPE *__capability ptr, TYPE *expected,	\
			 TYPE desired)					\
  {									\
    return __atomic_compare_exchange_n (ptr, expected, desired,		\
					0, __ATOMIC_ACQUIRE,		\
					__ATOMIC_RELAXED);		\
  }									\
									\
  TYPE									\
  test_##TYPE##_release (TYPE *__capability ptr, TYPE *expected,	\
			 TYPE desired)					\
  {									\
    return __atomic_compare_exchange_n (ptr, expected, desired,		\
					0, __ATOMIC_RELEASE,		\
					__ATOMIC_RELAXED);		\
  }									\
									\
  TYPE									\
  test_##TYPE##_seq_cst (TYPE *__capability ptr, TYPE *expected,	\
			 TYPE desired)					\
  {									\
    return __atomic_compare_exchange_n (ptr, expected, desired,		\
					0, __ATOMIC_SEQ_CST,		\
					__ATOMIC_SEQ_CST);		\
  }

TEST_SIZE (uint8_t, 1)
TEST_SIZE (uint16_t, 2)
TEST_SIZE (uint32_t, 4)
TEST_SIZE (uint64_t, 8)
TEST_SIZE (uint128, 16)
TEST_SIZE (intcap, capability)

/* { dg-final { scan-assembler-times {\t__atomic_compare_exchange_1_c} 4 } } */
/* { dg-final { scan-assembler-times {\t__atomic_compare_exchange_2_c} 4 } } */
/* { dg-final { scan-assembler-times {\t__atomic_compare_exchange_4_c} 4 } } */
/* { dg-final { scan-assembler-times {\t__atomic_compare_exchange_8_c} 4 } } */
/* { dg-final { scan-assembler-times {\t__atomic_compare_exchange_16_c} 4 } } */
/* { dg-final { scan-assembler-times {\t__atomic_compare_exchange_capability_c} 4 } } */
