/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_SIZE(TYPE, SIZE)						\
  TYPE									\
  test_##TYPE##_relaxed (TYPE *__capability ptr, TYPE val)		\
  {									\
    TYPE res;								\
    __atomic_exchange (ptr, &val, &res, __ATOMIC_RELAXED);		\
    return res;								\
  }									\
									\
  TYPE									\
  test_##TYPE##_acquire (TYPE *__capability ptr, TYPE val)		\
  {									\
    TYPE res;								\
    __atomic_exchange (ptr, &val, &res, __ATOMIC_ACQUIRE);		\
    return res;								\
  }									\
									\
  TYPE									\
  test_##TYPE##_release (TYPE *__capability ptr, TYPE val)		\
  {									\
    TYPE res;								\
    __atomic_exchange (ptr, &val, &res, __ATOMIC_RELEASE);		\
    return res;								\
  }									\
									\
  TYPE									\
  test_##TYPE##_acq_rel (TYPE *__capability ptr, TYPE val)		\
  {									\
    TYPE res;								\
    __atomic_exchange (ptr, &val, &res, __ATOMIC_ACQ_REL);		\
    return res;								\
  }									\
									\
  TYPE									\
  test_##TYPE##_seq_cst (TYPE *__capability ptr, TYPE val)		\
  {									\
    TYPE res;								\
    __atomic_exchange (ptr, &val, &res, __ATOMIC_SEQ_CST);		\
    return res;								\
  }

TEST_SIZE (uint8_t, 1)
TEST_SIZE (uint16_t, 2)
TEST_SIZE (uint32_t, 4)
TEST_SIZE (uint64_t, 8)
TEST_SIZE (uint128, 16)
TEST_SIZE (intcap, capability)

/* { dg-final { scan-assembler-times {\t__atomic_exchange_1_c} 5 } } */
/* { dg-final { scan-assembler-times {\t__atomic_exchange_2_c} 5 } } */
/* { dg-final { scan-assembler-times {\t__atomic_exchange_4_c} 5 } } */
/* { dg-final { scan-assembler-times {\t__atomic_exchange_8_c} 5 } } */
/* { dg-final { scan-assembler-times {\t__atomic_exchange_16_c} 5 } } */
/* { dg-final { scan-assembler-times {\t__atomic_exchange_capability_c} 5 } } */
