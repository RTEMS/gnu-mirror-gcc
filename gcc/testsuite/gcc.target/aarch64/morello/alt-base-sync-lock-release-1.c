/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps -Wno-cheri-implicit-pointer-conversion-from-cap -Wno-cheri-explicit-pointer-conversion-from-cap" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;

#define TEST_SIZE(TYPE, SIZE)				\
  void							\
  test_##TYPE (TYPE *__capability ptr)			\
  {							\
    __sync_lock_release_##SIZE##_c (ptr);		\
  }							\
							\
  TYPE							\
  test_##TYPE##_convert (TYPE *__capability ptr)	\
  {							\
    __sync_lock_release_##SIZE ((TYPE *) ptr);		\
  }

/*
** test_uint8_t:
**	stlrb	wzr, \[c0\]
**	ret
*/

/*
** test_uint8_t_convert:
**	stlrb	wzr, \[x0\]
**	ret
*/
TEST_SIZE (uint8_t, 1)

/*
** test_uint16_t:
**	dmb	ish
**	strh	wzr, \[c0\]
**	ret
*/

/*
** test_uint16_t_convert:
**	stlrh	wzr, \[x0\]
**	ret
*/

TEST_SIZE (uint16_t, 2)

/*
** test_uint32_t:
**	stlr	wzr, \[c0\]
**	ret
*/

/*
** test_uint32_t_convert:
**	stlr	wzr, \[x0\]
**	ret
*/
TEST_SIZE (uint32_t, 4)

/*
** test_uint64_t:
**	dmb	ish
**	str	xzr, \[c0\]
**	ret
*/

/*
** test_uint64_t_convert:
**	stlr	xzr, \[x0\]
**	ret
*/
TEST_SIZE (uint64_t, 8)

/*
** test_uint128:
**	stlr	czr, \[c0\]
**	ret
*/

/*
** test_uint128_convert:
**	stlr	czr, \[x0\]
**	ret
*/
TEST_SIZE (uint128, 16)
