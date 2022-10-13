/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps -Wno-cheri-implicit-pointer-conversion-from-cap -Wno-cheri-explicit-pointer-conversion-from-cap" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_OPERATION(TYPE, SIZE, OPERATION)				\
  TYPE									\
  test_##TYPE##_fetch_and_##OPERATION (TYPE *__capability ptr, TYPE val) \
  {									\
    return __sync_fetch_and_##OPERATION##_##SIZE ((TYPE *) ptr, val);	\
  }									\
									\
  TYPE									\
  test_##TYPE##_##OPERATION##_and_fetch (TYPE *__capability ptr, TYPE val) \
  {									\
    return __sync_##OPERATION##_and_fetch_##SIZE ((TYPE *) ptr, val);	\
  }

#define TEST_SIZE(TYPE, SIZE)				\
  TEST_OPERATION (TYPE, SIZE, add)			\
  TEST_OPERATION (TYPE, SIZE, sub)			\
  TEST_OPERATION (TYPE, SIZE, and)			\
  TEST_OPERATION (TYPE, SIZE, nand)			\
  TEST_OPERATION (TYPE, SIZE, or)			\
  TEST_OPERATION (TYPE, SIZE, xor)

/*
** test_uint8_t_fetch_and_add:
**	ldaddalb	w1, w0, \[x0\]
**	ret
*/

/*
** test_uint8_t_fetch_and_sub:
**	...
**	ldaddalb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_and:
**	...
**	ldclralb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_nand:
**	...
**	ldxrb	.*
**	...
**	stlxrb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_or:
**	...
**	ldsetalb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_xor:
**	...
**	ldeoralb	.*
**	...
*/

/*
** test_uint8_t_add_and_fetch:
**	...
**	ldaddalb	w1, (w[0-9]+), \[x0\]
**	add	w0, (w1, \1|\1, w1)
**	ret
*/

/*
** test_uint8_t_sub_and_fetch:
**	...
**	ldaddalb	.*
**	...
*/

/*
** test_uint8_t_and_and_fetch:
**	...
**	ldclralb	.*
**	...
*/

/*
** test_uint8_t_nand_and_fetch:
**	...
**	ldxrb	.*
**	...
**	stlxrb	.*
**	...
*/

/*
** test_uint8_t_or_and_fetch:
**	...
**	ldsetalb	.*
**	...
*/

/*
** test_uint8_t_xor_and_fetch:
**	...
**	ldeoralb	.*
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
