/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-skip-if "" { *-*-* } { "-mfake-capability" } { "" } }  */

#include <stdint.h>

typedef __uint128_t uint128;

#define TEST_SIZE(TYPE)			\
  void					\
  test_##TYPE (TYPE *ptr)		\
  {					\
    __sync_lock_release (ptr);		\
  }

/*
** test_uint8_t:
**	stlrb	wzr, \[[xc]0\]
**	ret
*/
TEST_SIZE (uint8_t)

/*
** test_uint16_t:
**	stlrh	wzr, \[[xc]0\]
**	ret
*/
TEST_SIZE (uint16_t)

/*
** test_uint32_t:
**	stlr	wzr, \[[xc]0\]
**	ret
*/
TEST_SIZE (uint32_t)

/*
** test_uint64_t:
**	stlr	xzr, \[[xc]0\]
**	ret
*/
TEST_SIZE (uint64_t)

/*
** test_uint128:
**	stlr	czr, \[[xc]0\]
**	ret
*/
TEST_SIZE (uint128)
