/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps -Wno-cheri-implicit-pointer-conversion-from-cap -Wno-cheri-explicit-pointer-conversion-from-cap" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_SIZE(TYPE, SIZE)						\
  TYPE									\
  test_##TYPE (TYPE *__capability ptr, TYPE val)		 	\
  {									\
    return __sync_lock_test_and_set_##SIZE ((TYPE *) ptr, val);		\
  }

/*
** test_uint8_t:
**	swpab	w1, w0, \[x0\]
**	ret
*/
TEST_SIZE (uint8_t, 1)

/*
** test_uint16_t:
**	swpah	w1, w0, \[x0\]
**	ret
*/
TEST_SIZE (uint16_t, 2)

/*
** test_uint32_t:
**	swpa	w1, w0, \[x0\]
**	ret
*/
TEST_SIZE (uint32_t, 4)

/*
** test_uint64_t:
**	swpa	x1, x0, \[x0\]
**	ret
*/
TEST_SIZE (uint64_t, 8)

/*
** test_uint128:
**	...
**	caspal	.*
**	...
*/
TEST_SIZE (uint128, 16)

/*
** test_intcap:
**	swpa	c1, c0, \[x0\]
**	ret
*/
TEST_SIZE (intcap, capability)
