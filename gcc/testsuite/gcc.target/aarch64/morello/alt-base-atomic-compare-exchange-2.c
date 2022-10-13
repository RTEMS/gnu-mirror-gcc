/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps -Wno-cheri-implicit-pointer-conversion-from-cap -Wno-cheri-explicit-pointer-conversion-from-cap" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_SIZE(TYPE, SIZE)						\
  TYPE									\
  test_##TYPE##_relaxed (TYPE *__capability ptr,			\
			 TYPE *__capability expected, TYPE desired)	\
  {									\
    return __atomic_compare_exchange_##SIZE ((TYPE *) ptr, expected,	\
					     desired, 0,		\
					     __ATOMIC_RELAXED,		\
					     __ATOMIC_RELAXED);		\
  }									\
									\
  TYPE									\
  test_##TYPE##_acquire (TYPE *__capability ptr,			\
			 TYPE *__capability expected, TYPE desired)	\
  {									\
    return __atomic_compare_exchange_##SIZE ((TYPE *) ptr, expected,	\
					     desired, 0,		\
					     __ATOMIC_ACQUIRE,		\
					     __ATOMIC_RELAXED);		\
  }									\
									\
  TYPE									\
  test_##TYPE##_release (TYPE *__capability ptr,			\
			 TYPE *__capability expected, TYPE desired)	\
  {									\
    return __atomic_compare_exchange_##SIZE ((TYPE *) ptr, expected,	\
					     desired, 0,		\
					     __ATOMIC_RELEASE,		\
					     __ATOMIC_RELAXED);		\
  }									\
									\
  TYPE									\
  test_##TYPE##_seq_cst (TYPE *__capability ptr,			\
			 TYPE *__capability expected, TYPE desired)	\
  {									\
    return __atomic_compare_exchange_##SIZE ((TYPE *) ptr, expected,	\
					     desired, 0,		\
					     __ATOMIC_SEQ_CST,		\
					     __ATOMIC_SEQ_CST);		\
  }

/*
** test_uint8_t_relaxed:
**	...
**	casb	.*
**	...
*/

/*
** test_uint8_t_acquire:
**	...
**	casab	.*
**	...
*/

/*
** test_uint8_t_release:
**	...
**	caslb	.*
**	...
*/

/*
** test_uint8_t_seq_cst:
**	...
**	casalb	.*
**	...
*/
TEST_SIZE (uint8_t, 1)

/* Don't match the rest.  The matches above are mostly to make sure
   that there are no typos in the function names.  */
TEST_SIZE (uint16_t, 2)
TEST_SIZE (uint32_t, 4)
TEST_SIZE (uint64_t, 8)
TEST_SIZE (uint128, 16)
TEST_SIZE (intcap, capability)
