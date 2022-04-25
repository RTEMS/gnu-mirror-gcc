/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-skip-if "" { *-*-* } { "-mfake-capability" } { "" } }  */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_OPERATION(TYPE, OPERATION)				\
  TYPE									\
  test_##TYPE##_fetch_and_##OPERATION (TYPE *__capability ptr, TYPE val) \
  {									\
    return __sync_fetch_and_##OPERATION ((TYPE *) ptr, val);		\
  }									\
									\
  TYPE									\
  test_##TYPE##_##OPERATION##_and_fetch (TYPE *__capability ptr, TYPE val) \
  {									\
    return __sync_##OPERATION##_and_fetch ((TYPE *) ptr, val);		\
  }

#define TEST_SIZE(TYPE)				\
  TEST_OPERATION (TYPE, add)			\
  TEST_OPERATION (TYPE, sub)			\
  TEST_OPERATION (TYPE, and)			\
  TEST_OPERATION (TYPE, nand)			\
  TEST_OPERATION (TYPE, or)			\
  TEST_OPERATION (TYPE, xor)

/*
** test_uint8_t_fetch_and_add:
**	...
**	ldaddalb	.*
**	...
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
**	ldaddalb	.*
**	...
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
TEST_SIZE (uint8_t)

/*
** test_uint16_t_fetch_and_add:
**	...
**	ldaddalh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_sub:
**	...
**	ldaddalh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_and:
**	...
**	ldclralh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_nand:
**	...
**	ldxrh	.*
**	...
**	stlxrh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_or:
**	...
**	ldsetalh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_xor:
**	...
**	ldeoralh	.*
**	...
*/

/*
** test_uint16_t_add_and_fetch:
**	...
**	ldaddalh	.*
**	...
*/

/*
** test_uint16_t_sub_and_fetch:
**	...
**	ldaddalh	.*
**	...
*/

/*
** test_uint16_t_and_and_fetch:
**	...
**	ldclralh	.*
**	...
*/

/*
** test_uint16_t_nand_and_fetch:
**	...
**	ldxrh	.*
**	...
**	stlxrh	.*
**	...
*/

/*
** test_uint16_t_or_and_fetch:
**	...
**	ldsetalh	.*
**	...
*/

/*
** test_uint16_t_xor_and_fetch:
**	...
**	ldeoralh	.*
**	...
*/
TEST_SIZE (uint16_t)

/*
** test_uint32_t_fetch_and_add:
**	...
**	ldaddal	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_sub:
**	...
**	ldaddal	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_and:
**	...
**	ldclral	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_nand:
**	...
**	ldxr	w[0-9]+, .*
**	...
**	stlxr	w[0-9]+, w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_or:
**	...
**	ldsetal	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_xor:
**	...
**	ldeoral	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_add_and_fetch:
**	...
**	ldaddal	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_sub_and_fetch:
**	...
**	ldaddal	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_and_and_fetch:
**	...
**	ldclral	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_nand_and_fetch:
**	...
**	ldxr	w[0-9]+, .*
**	...
**	stlxr	w[0-9]+, w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_or_and_fetch:
**	...
**	ldsetal	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_xor_and_fetch:
**	...
**	ldeoral	w[0-9]+, .*
**	...
*/
TEST_SIZE (uint32_t)

/*
** test_uint64_t_fetch_and_add:
**	...
**	ldaddal	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_sub:
**	...
**	ldaddal	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_and:
**	...
**	ldclral	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_nand:
**	...
**	ldxr	x[0-9]+, .*
**	...
**	stlxr	w[0-9]+, x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_or:
**	...
**	ldsetal	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_xor:
**	...
**	ldeoral	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_add_and_fetch:
**	...
**	ldaddal	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_sub_and_fetch:
**	...
**	ldaddal	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_and_and_fetch:
**	...
**	ldclral	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_nand_and_fetch:
**	...
**	ldxr	x[0-9]+, .*
**	...
**	stlxr	w[0-9]+, x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_or_and_fetch:
**	...
**	ldsetal	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_xor_and_fetch:
**	...
**	ldeoral	x[0-9]+, .*
**	...
*/
TEST_SIZE (uint64_t)

/* Must compile, but don't match the result.  */
TEST_SIZE (uint128)

/*
** test_intcap_fetch_and_add:
**	...
**	ldxr	(c[0-9]+), \[([xc][0-9]+)\]
**	add	(c[0-9]+), \1, x[0-9]+
**	stlxr	(w[0-9]+), \3, \[\2\]
**	cbnz	\4, .*
**	dmb	ish
**	...
*/

/*
** test_intcap_fetch_and_sub:
**	...
**	ldxr	.*
**	sub	.*
**	scvalue	.*
**	stlxr	.*
**	cbnz	.*
**	dmb	ish
**	...
*/

/*
** test_intcap_fetch_and_and:
**	...
**	ldxr	.*
**	and	.*
**	scvalue	.*
**	stlxr	.*
**	cbnz	.*
**	dmb	ish
**	...
*/

/*
** test_intcap_fetch_and_nand:
**	...
**	ldxr	.*
**	.*
**	stlxr	.*
**	cbnz	.*
**	dmb	ish
**	...
*/

/*
** test_intcap_fetch_and_or:
**	...
**	ldxr	.*
**	orr	.*
**	scvalue	.*
**	stlxr	.*
**	cbnz	.*
**	dmb	ish
**	...
*/

/*
** test_intcap_fetch_and_xor:
**	...
**	ldxr	.*
**	eor	.*
**	scvalue	.*
**	stlxr	.*
**	cbnz	.*
**	...
*/

/* Don't match the rest since they currently include a redundant final
   operation.  */
TEST_SIZE (intcap)
