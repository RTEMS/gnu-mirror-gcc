/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps -Wno-cheri-implicit-pointer-conversion-from-cap -Wno-cheri-explicit-pointer-conversion-from-cap" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_OPERATION(TYPE, SIZE, OPERATION)				\
  TYPE									\
  test_##TYPE##_fetch_##OPERATION (TYPE *__capability ptr, TYPE val)	\
  {									\
    return __atomic_fetch_##OPERATION##_##SIZE ((TYPE *) ptr, val,	\
						__ATOMIC_RELAXED);	\
  }									\
									\
  TYPE									\
  test_##TYPE##_##OPERATION##_fetch (TYPE *__capability ptr, TYPE val)	\
  {									\
    return __atomic_##OPERATION##_fetch_##SIZE ((TYPE *) ptr, val,	\
						__ATOMIC_RELAXED);	\
  }

#define TEST_SIZE(TYPE, SIZE)				\
  TEST_OPERATION (TYPE, SIZE, add)			\
  TEST_OPERATION (TYPE, SIZE, sub)			\
  TEST_OPERATION (TYPE, SIZE, and)			\
  TEST_OPERATION (TYPE, SIZE, nand)			\
  TEST_OPERATION (TYPE, SIZE, or)			\
  TEST_OPERATION (TYPE, SIZE, xor)

/*
** test_uint8_t_fetch_add:
**	ldaddb	w1, w0, \[x0\]
**	ret
*/

/*
** test_uint8_t_fetch_sub:
**	...
**	neg	.*
**	ldaddb	.*
**	ret
*/

/*
** test_uint8_t_fetch_and:
**	...
**	mvn	.*
**	ldclrb	.*
**	ret
*/

/*
** test_uint8_t_fetch_nand:
**	...
**	ldxrb	.*
**	...
**	stxrb	.*
**	...
**	ret
*/

/*
** test_uint8_t_fetch_or:
**	ldsetb	w1, w0, \[x0\]
**	ret
*/

/*
** test_uint8_t_fetch_xor:
**	ldeorb	w1, w0, \[x0\]
**	ret
*/

/*
** test_uint8_t_add_fetch:
**	...
**	ldaddb	.*
**	add	w0, .*
**	ret
*/

/*
** test_uint8_t_sub_fetch:
**	...
**	ldaddb	.*
**	sub	w0, .*
**	ret
*/

/*
** test_uint8_t_and_fetch:
**	...
**	ldclrb	.*
**	and	w0, .*
**	ret
*/

/*
** test_uint8_t_nand_fetch:
**	...
**	ldxrb	.*
**	...
**	stxrb	.*
**	...
**	ret
*/

/*
** test_uint8_t_or_fetch:
**	...
**	ldsetb	.*
**	orr	w0, .*
**	ret
*/

/*
** test_uint8_t_xor_fetch:
**	...
**	ldeorb	.*
**	eor	w0, .*
**	ret
*/
TEST_SIZE (uint8_t, 1)

/*
** test_uint16_t_fetch_add:
**	ldaddh	w1, w0, \[x0\]
**	ret
*/

/*
** test_uint16_t_fetch_sub:
**	...
**	neg	.*
**	ldaddh	.*
**	ret
*/

/*
** test_uint16_t_fetch_and:
**	...
**	mvn	.*
**	ldclrh	.*
**	ret
*/

/*
** test_uint16_t_fetch_nand:
**	...
**	ldxrh	.*
**	...
**	stxrh	.*
**	...
**	ret
*/

/*
** test_uint16_t_fetch_or:
**	ldseth	w1, w0, \[x0\]
**	ret
*/

/*
** test_uint16_t_fetch_xor:
**	ldeorh	w1, w0, \[x0\]
**	ret
*/

/*
** test_uint16_t_add_fetch:
**	...
**	ldaddh	.*
**	add	w0, .*
**	ret
*/

/*
** test_uint16_t_sub_fetch:
**	...
**	ldaddh	.*
**	sub	w0, .*
**	ret
*/

/*
** test_uint16_t_and_fetch:
**	...
**	ldclrh	.*
**	and	w0, .*
**	ret
*/

/*
** test_uint16_t_nand_fetch:
**	...
**	ldxrh	.*
**	...
**	stxrh	.*
**	...
**	ret
*/

/*
** test_uint16_t_or_fetch:
**	...
**	ldseth	.*
**	orr	w0, .*
**	ret
*/

/*
** test_uint16_t_xor_fetch:
**	...
**	ldeorh	.*
**	eor	w0, .*
**	ret
*/
TEST_SIZE (uint16_t, 2)

/*
** test_uint32_t_fetch_add:
**	ldadd	w1, w0, \[x0\]
**	ret
*/

/*
** test_uint32_t_fetch_sub:
**	...
**	neg	.*
**	ldadd	.*
**	ret
*/

/*
** test_uint32_t_fetch_and:
**	...
**	mvn	.*
**	ldclr	.*
**	ret
*/

/*
** test_uint32_t_fetch_nand:
**	...
**	ldxr	.*
**	...
**	stxr	.*
**	...
**	ret
*/

/*
** test_uint32_t_fetch_or:
**	ldset	w1, w0, \[x0\]
**	ret
*/

/*
** test_uint32_t_fetch_xor:
**	ldeor	w1, w0, \[x0\]
**	ret
*/

/*
** test_uint32_t_add_fetch:
**	...
**	ldadd	.*
**	add	w0, .*
**	ret
*/

/*
** test_uint32_t_sub_fetch:
**	...
**	ldadd	.*
**	sub	w0, .*
**	ret
*/

/*
** test_uint32_t_and_fetch:
**	...
**	ldclr	.*
**	and	w0, .*
**	ret
*/

/*
** test_uint32_t_nand_fetch:
**	...
**	ldxr	.*
**	...
**	stxr	.*
**	...
**	ret
*/

/*
** test_uint32_t_or_fetch:
**	...
**	ldset	.*
**	orr	w0, .*
**	ret
*/

/*
** test_uint32_t_xor_fetch:
**	...
**	ldeor	.*
**	eor	w0, .*
**	ret
*/
TEST_SIZE (uint32_t, 4)

/*
** test_uint64_t_fetch_add:
**	ldadd	x1, x0, \[x0\]
**	ret
*/

/*
** test_uint64_t_fetch_sub:
**	...
**	neg	.*
**	ldadd	.*
**	ret
*/

/*
** test_uint64_t_fetch_and:
**	...
**	mvn	.*
**	ldclr	.*
**	ret
*/

/*
** test_uint64_t_fetch_nand:
**	...
**	ldxr	.*
**	...
**	stxr	.*
**	...
**	ret
*/

/*
** test_uint64_t_fetch_or:
**	ldset	x1, x0, \[x0\]
**	ret
*/

/*
** test_uint64_t_fetch_xor:
**	ldeor	x1, x0, \[x0\]
**	ret
*/

/*
** test_uint64_t_add_fetch:
**	...
**	ldadd	.*
**	add	x0, .*
**	ret
*/

/*
** test_uint64_t_sub_fetch:
**	...
**	ldadd	.*
**	sub	x0, .*
**	ret
*/

/*
** test_uint64_t_and_fetch:
**	...
**	ldclr	.*
**	and	x0, .*
**	ret
*/

/*
** test_uint64_t_nand_fetch:
**	...
**	ldxr	.*
**	...
**	stxr	.*
**	...
**	ret
*/

/*
** test_uint64_t_or_fetch:
**	...
**	ldset	.*
**	orr	x0, .*
**	ret
*/

/*
** test_uint64_t_xor_fetch:
**	...
**	ldeor	.*
**	eor	x0, .*
**	ret
*/
TEST_SIZE (uint64_t, 8)

/* Not matched, calls into libatomic.  */
TEST_SIZE (uint128, 16)

/*
** test_intcap_fetch_add:
**	...
**	ldxr	c0, \[(x[0-9]+)\]
**	add	(c[0-9]+), c0, x[0-9]+
**	stxr	(w[0-9]+), \2, \[\1\]
**	cbnz	\3, .*
**	ret
*/

/*
** test_intcap_fetch_sub:
**	...
**	ldxr	.*
**	sub	.*
**	scvalue	.*
**	stxr	.*
**	cbnz	.*
**	ret
*/

/*
** test_intcap_fetch_and:
**	...
**	ldxr	.*
**	and	.*
**	scvalue	.*
**	stxr	.*
**	cbnz	.*
**	ret
*/

/*
** test_intcap_fetch_nand:
**	...
**	ldxr	.*
**	.*
**	stxr	.*
**	cbnz	.*
**	ret
*/

/*
** test_intcap_fetch_or:
**	...
**	ldxr	.*
**	orr	.*
**	scvalue	.*
**	stxr	.*
**	cbnz	.*
**	ret
*/

/*
** test_intcap_fetch_xor:
**	...
**	ldxr	.*
**	eor	.*
**	scvalue	.*
**	stxr	.*
**	cbnz	.*
**	ret
*/

/* Don't match the rest since they currently include a redundant final
   operation.  */
TEST_SIZE (intcap, capability)
