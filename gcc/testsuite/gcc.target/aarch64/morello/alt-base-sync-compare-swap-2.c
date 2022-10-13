/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps -Wno-cheri-implicit-pointer-conversion-from-cap -Wno-cheri-explicit-pointer-conversion-from-cap" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_SIZE(TYPE, SIZE)						\
  _Bool									\
  bool_##TYPE (TYPE *__capability ptr, TYPE oldval, TYPE newval) 	\
  {									\
    return __sync_bool_compare_and_swap_##SIZE ((TYPE *) ptr, oldval,	\
						newval);		\
  }									\
									\
  TYPE									\
  val_##TYPE (TYPE oldval, TYPE newval, TYPE *__capability ptr)		\
  {									\
    return __sync_val_compare_and_swap_##SIZE ((TYPE *) ptr, oldval,	\
					       newval);			\
  }

/*
** bool_uint8_t:
**	...
**	casalb	.*
**	...
*/

/*
** val_uint8_t:
**	...
**	casalb	.*
**	...
*/
TEST_SIZE (uint8_t, 1)

/*
** bool_uint16_t:
**	...
**	casalh	.*
**	...
*/

/*
** val_uint16_t:
**	...
**	casalh	.*
**	...
*/
TEST_SIZE (uint16_t, 2)

/*
** bool_uint32_t:
**	...
**	casal	.*
**	...
*/

/*
** val_uint32_t:
**	casal	w0, w1, \[x2\]
**	ret
*/
TEST_SIZE (uint32_t, 4)

/*
** bool_uint64_t:
**	...
**	casal	.*
**	...
*/

/*
** val_uint64_t:
**	casal	x0, x1, \[x2\]
**	ret
*/
TEST_SIZE (uint64_t, 8)

/*
** bool_uint128:
**	...
**	caspal	.*
**	...
*/

/*
** val_uint128:
**	...
**	caspal	.*
**	...
*/
TEST_SIZE (uint128, 16)

/*
** bool_intcap:
**	...
**	casal	.*
**	...
*/

/*
** val_intcap:
**	casal	c0, c1, \[x2\]
**	ret
*/
TEST_SIZE (intcap, capability)
