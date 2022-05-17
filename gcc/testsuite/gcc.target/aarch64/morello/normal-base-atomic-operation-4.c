/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -Wno-cheri-explicit-pointer-conversion-from-cap -save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-skip-if "" { *-*-* } { "-mfake-capability" } { "" } }  */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_OPERATION(TYPE, OPERATION)					\
  TYPE									\
  test_##TYPE##_fetch_and_##OPERATION (TYPE *__capability ptr, TYPE val) \
  {									\
    return __atomic_fetch_##OPERATION ((TYPE *) ptr, val,		\
				       __ATOMIC_RELEASE);		\
  }									\
									\
  TYPE									\
  test_##TYPE##_##OPERATION##_and_fetch (TYPE *__capability ptr, TYPE val) \
  {									\
    return __atomic_##OPERATION##_fetch ((TYPE *) ptr, val,		\
					 __ATOMIC_RELEASE);		\
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
**	ldaddlb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_sub:
**	...
**	ldaddlb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_and:
**	...
**	ldclrlb	.*
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
**	ldsetlb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_xor:
**	...
**	ldeorlb	.*
**	...
*/

/*
** test_uint8_t_add_and_fetch:
**	...
**	ldaddlb	.*
**	...
*/

/*
** test_uint8_t_sub_and_fetch:
**	...
**	ldaddlb	.*
**	...
*/

/*
** test_uint8_t_and_and_fetch:
**	...
**	ldclrlb	.*
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
**	ldsetlb	.*
**	...
*/

/*
** test_uint8_t_xor_and_fetch:
**	...
**	ldeorlb	.*
**	...
*/
TEST_SIZE (uint8_t)

/*
** test_uint16_t_fetch_and_add:
**	...
**	ldaddlh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_sub:
**	...
**	ldaddlh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_and:
**	...
**	ldclrlh	.*
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
**	ldsetlh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_xor:
**	...
**	ldeorlh	.*
**	...
*/

/*
** test_uint16_t_add_and_fetch:
**	...
**	ldaddlh	.*
**	...
*/

/*
** test_uint16_t_sub_and_fetch:
**	...
**	ldaddlh	.*
**	...
*/

/*
** test_uint16_t_and_and_fetch:
**	...
**	ldclrlh	.*
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
**	ldsetlh	.*
**	...
*/

/*
** test_uint16_t_xor_and_fetch:
**	...
**	ldeorlh	.*
**	...
*/
TEST_SIZE (uint16_t)

/*
** test_uint32_t_fetch_and_add:
**	...
**	ldaddl	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_sub:
**	...
**	ldaddl	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_and:
**	...
**	ldclrl	w[0-9]+, .*
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
**	ldsetl	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_xor:
**	...
**	ldeorl	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_add_and_fetch:
**	...
**	ldaddl	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_sub_and_fetch:
**	...
**	ldaddl	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_and_and_fetch:
**	...
**	ldclrl	w[0-9]+, .*
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
**	ldsetl	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_xor_and_fetch:
**	...
**	ldeorl	w[0-9]+, .*
**	...
*/
TEST_SIZE (uint32_t)

/*
** test_uint64_t_fetch_and_add:
**	...
**	ldaddl	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_sub:
**	...
**	ldaddl	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_and:
**	...
**	ldclrl	x[0-9]+, .*
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
**	ldsetl	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_xor:
**	...
**	ldeorl	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_add_and_fetch:
**	...
**	ldaddl	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_sub_and_fetch:
**	...
**	ldaddl	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_and_and_fetch:
**	...
**	ldclrl	x[0-9]+, .*
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
**	ldsetl	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_xor_and_fetch:
**	...
**	ldeorl	x[0-9]+, .*
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
**	...
*/

/*
** test_intcap_fetch_and_nand:
**	...
**	ldxr	.*
**	.*
**	stlxr	.*
**	cbnz	.*
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

/* { dg-final { scan-assembler-not {\tdmb\t} } } */

/* { dg-final { scan-assembler-not {\tdmb\t} } } */
