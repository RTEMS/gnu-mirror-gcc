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
				       __ATOMIC_RELAXED);		\
  }									\
									\
  TYPE									\
  test_##TYPE##_##OPERATION##_and_fetch (TYPE *__capability ptr, TYPE val) \
  {									\
    return __atomic_##OPERATION##_fetch ((TYPE *) ptr, val,		\
					 __ATOMIC_RELAXED);		\
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
**	ldaddb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_sub:
**	...
**	ldaddb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_and:
**	...
**	ldclrb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_nand:
**	...
**	ldxrb	.*
**	...
**	stxrb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_or:
**	...
**	ldsetb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_xor:
**	...
**	ldeorb	.*
**	...
*/

/*
** test_uint8_t_add_and_fetch:
**	...
**	ldaddb	.*
**	...
*/

/*
** test_uint8_t_sub_and_fetch:
**	...
**	ldaddb	.*
**	...
*/

/*
** test_uint8_t_and_and_fetch:
**	...
**	ldclrb	.*
**	...
*/

/*
** test_uint8_t_nand_and_fetch:
**	...
**	ldxrb	.*
**	...
**	stxrb	.*
**	...
*/

/*
** test_uint8_t_or_and_fetch:
**	...
**	ldsetb	.*
**	...
*/

/*
** test_uint8_t_xor_and_fetch:
**	...
**	ldeorb	.*
**	...
*/
TEST_SIZE (uint8_t)

/*
** test_uint16_t_fetch_and_add:
**	...
**	ldaddh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_sub:
**	...
**	ldaddh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_and:
**	...
**	ldclrh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_nand:
**	...
**	ldxrh	.*
**	...
**	stxrh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_or:
**	...
**	ldseth	.*
**	...
*/

/*
** test_uint16_t_fetch_and_xor:
**	...
**	ldeorh	.*
**	...
*/

/*
** test_uint16_t_add_and_fetch:
**	...
**	ldaddh	.*
**	...
*/

/*
** test_uint16_t_sub_and_fetch:
**	...
**	ldaddh	.*
**	...
*/

/*
** test_uint16_t_and_and_fetch:
**	...
**	ldclrh	.*
**	...
*/

/*
** test_uint16_t_nand_and_fetch:
**	...
**	ldxrh	.*
**	...
**	stxrh	.*
**	...
*/

/*
** test_uint16_t_or_and_fetch:
**	...
**	ldseth	.*
**	...
*/

/*
** test_uint16_t_xor_and_fetch:
**	...
**	ldeorh	.*
**	...
*/
TEST_SIZE (uint16_t)

/*
** test_uint32_t_fetch_and_add:
**	...
**	ldadd	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_sub:
**	...
**	ldadd	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_and:
**	...
**	ldclr	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_nand:
**	...
**	ldxr	w[0-9]+, .*
**	...
**	stxr	w[0-9]+, w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_or:
**	...
**	ldset	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_xor:
**	...
**	ldeor	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_add_and_fetch:
**	...
**	ldadd	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_sub_and_fetch:
**	...
**	ldadd	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_and_and_fetch:
**	...
**	ldclr	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_nand_and_fetch:
**	...
**	ldxr	w[0-9]+, .*
**	...
**	stxr	w[0-9]+, w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_or_and_fetch:
**	...
**	ldset	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_xor_and_fetch:
**	...
**	ldeor	w[0-9]+, .*
**	...
*/
TEST_SIZE (uint32_t)

/*
** test_uint64_t_fetch_and_add:
**	...
**	ldadd	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_sub:
**	...
**	ldadd	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_and:
**	...
**	ldclr	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_nand:
**	...
**	ldxr	x[0-9]+, .*
**	...
**	stxr	w[0-9]+, x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_or:
**	...
**	ldset	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_xor:
**	...
**	ldeor	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_add_and_fetch:
**	...
**	ldadd	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_sub_and_fetch:
**	...
**	ldadd	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_and_and_fetch:
**	...
**	ldclr	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_nand_and_fetch:
**	...
**	ldxr	x[0-9]+, .*
**	...
**	stxr	w[0-9]+, x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_or_and_fetch:
**	...
**	ldset	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_xor_and_fetch:
**	...
**	ldeor	x[0-9]+, .*
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
**	stxr	(w[0-9]+), \3, \[\2\]
**	cbnz	\4, .*
**	...
*/

/*
** test_intcap_fetch_and_sub:
**	...
**	ldxr	.*
**	sub	.*
**	scvalue	.*
**	stxr	.*
**	cbnz	.*
**	...
*/

/*
** test_intcap_fetch_and_and:
**	...
**	ldxr	.*
**	and	.*
**	scvalue	.*
**	stxr	.*
**	cbnz	.*
**	...
*/

/*
** test_intcap_fetch_and_nand:
**	...
**	ldxr	.*
**	.*
**	stxr	.*
**	cbnz	.*
**	...
*/

/*
** test_intcap_fetch_and_or:
**	...
**	ldxr	.*
**	orr	.*
**	scvalue	.*
**	stxr	.*
**	cbnz	.*
**	...
*/

/*
** test_intcap_fetch_and_xor:
**	...
**	ldxr	.*
**	eor	.*
**	scvalue	.*
**	stxr	.*
**	cbnz	.*
**	...
*/

/* Don't match the rest since they currently include a redundant final
   operation.  */
TEST_SIZE (intcap)

/* { dg-final { scan-assembler-not {\tdmb\t} } } */
