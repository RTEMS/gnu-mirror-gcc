/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_SIZE(TYPE, SIZE)						\
  TYPE									\
  test_##TYPE (TYPE *__capability ptr, TYPE val)		 	\
  {									\
    return __sync_lock_test_and_set_##SIZE##_c (ptr, val);		\
  }

/*
** test_uint8_t:
**	b	__sync_lock_test_and_set_1_c
*/
TEST_SIZE (uint8_t, 1)

/*
** test_uint16_t:
**	b	__sync_lock_test_and_set_2_c
*/
TEST_SIZE (uint16_t, 2)

/*
** test_uint32_t:
**	b	__sync_lock_test_and_set_4_c
*/
TEST_SIZE (uint32_t, 4)

/*
** test_uint64_t:
**	b	__sync_lock_test_and_set_8_c
*/
TEST_SIZE (uint64_t, 8)

/*
** test_uint128:
**	b	__sync_lock_test_and_set_16_c
*/
TEST_SIZE (uint128, 16)

/*
** test_intcap:
**	b	__sync_lock_test_and_set_capability_c
*/
TEST_SIZE (intcap, capability)
