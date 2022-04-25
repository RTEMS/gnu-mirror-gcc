/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps" } */
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
				       __ATOMIC_ACQUIRE);		\
  }									\
									\
  TYPE									\
  test_##TYPE##_##OPERATION##_and_fetch (TYPE *__capability ptr, TYPE val) \
  {									\
    return __atomic_##OPERATION##_fetch ((TYPE *) ptr, val,		\
					 __ATOMIC_ACQUIRE);		\
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
**	ldaddab	.*
**	...
*/

/*
** test_uint8_t_fetch_and_sub:
**	...
**	ldaddab	.*
**	...
*/

/*
** test_uint8_t_fetch_and_and:
**	...
**	ldclrab	.*
**	...
*/

/*
** test_uint8_t_fetch_and_nand:
**	...
**	ldaxrb	.*
**	...
**	stxrb	.*
**	...
*/

/*
** test_uint8_t_fetch_and_or:
**	...
**	ldsetab	.*
**	...
*/

/*
** test_uint8_t_fetch_and_xor:
**	...
**	ldeorab	.*
**	...
*/

/*
** test_uint8_t_add_and_fetch:
**	...
**	ldaddab	.*
**	...
*/

/*
** test_uint8_t_sub_and_fetch:
**	...
**	ldaddab	.*
**	...
*/

/*
** test_uint8_t_and_and_fetch:
**	...
**	ldclrab	.*
**	...
*/

/*
** test_uint8_t_nand_and_fetch:
**	...
**	ldaxrb	.*
**	...
**	stxrb	.*
**	...
*/

/*
** test_uint8_t_or_and_fetch:
**	...
**	ldsetab	.*
**	...
*/

/*
** test_uint8_t_xor_and_fetch:
**	...
**	ldeorab	.*
**	...
*/
TEST_SIZE (uint8_t)

/*
** test_uint16_t_fetch_and_add:
**	...
**	ldaddah	.*
**	...
*/

/*
** test_uint16_t_fetch_and_sub:
**	...
**	ldaddah	.*
**	...
*/

/*
** test_uint16_t_fetch_and_and:
**	...
**	ldclrah	.*
**	...
*/

/*
** test_uint16_t_fetch_and_nand:
**	...
**	ldaxrh	.*
**	...
**	stxrh	.*
**	...
*/

/*
** test_uint16_t_fetch_and_or:
**	...
**	ldsetah	.*
**	...
*/

/*
** test_uint16_t_fetch_and_xor:
**	...
**	ldeorah	.*
**	...
*/

/*
** test_uint16_t_add_and_fetch:
**	...
**	ldaddah	.*
**	...
*/

/*
** test_uint16_t_sub_and_fetch:
**	...
**	ldaddah	.*
**	...
*/

/*
** test_uint16_t_and_and_fetch:
**	...
**	ldclrah	.*
**	...
*/

/*
** test_uint16_t_nand_and_fetch:
**	...
**	ldaxrh	.*
**	...
**	stxrh	.*
**	...
*/

/*
** test_uint16_t_or_and_fetch:
**	...
**	ldsetah	.*
**	...
*/

/*
** test_uint16_t_xor_and_fetch:
**	...
**	ldeorah	.*
**	...
*/
TEST_SIZE (uint16_t)

/*
** test_uint32_t_fetch_and_add:
**	...
**	ldadda	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_sub:
**	...
**	ldadda	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_and:
**	...
**	ldclra	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_nand:
**	...
**	ldaxr	w[0-9]+, .*
**	...
**	stxr	w[0-9]+, w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_or:
**	...
**	ldseta	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_fetch_and_xor:
**	...
**	ldeora	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_add_and_fetch:
**	...
**	ldadda	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_sub_and_fetch:
**	...
**	ldadda	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_and_and_fetch:
**	...
**	ldclra	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_nand_and_fetch:
**	...
**	ldaxr	w[0-9]+, .*
**	...
**	stxr	w[0-9]+, w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_or_and_fetch:
**	...
**	ldseta	w[0-9]+, .*
**	...
*/

/*
** test_uint32_t_xor_and_fetch:
**	...
**	ldeora	w[0-9]+, .*
**	...
*/
TEST_SIZE (uint32_t)

/*
** test_uint64_t_fetch_and_add:
**	...
**	ldadda	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_sub:
**	...
**	ldadda	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_and:
**	...
**	ldclra	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_nand:
**	...
**	ldaxr	x[0-9]+, .*
**	...
**	stxr	w[0-9]+, x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_or:
**	...
**	ldseta	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_fetch_and_xor:
**	...
**	ldeora	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_add_and_fetch:
**	...
**	ldadda	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_sub_and_fetch:
**	...
**	ldadda	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_and_and_fetch:
**	...
**	ldclra	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_nand_and_fetch:
**	...
**	ldaxr	x[0-9]+, .*
**	...
**	stxr	w[0-9]+, x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_or_and_fetch:
**	...
**	ldseta	x[0-9]+, .*
**	...
*/

/*
** test_uint64_t_xor_and_fetch:
**	...
**	ldeora	x[0-9]+, .*
**	...
*/
TEST_SIZE (uint64_t)

/* Must compile, but don't match the result.  */
TEST_SIZE (uint128)

/*
** test_intcap_fetch_and_add:
**	...
**	ldaxr	(c[0-9]+), \[([xc][0-9]+)\]
**	add	(c[0-9]+), \1, x[0-9]+
**	stxr	(w[0-9]+), \3, \[\2\]
**	cbnz	\4, .*
**	...
*/

/*
** test_intcap_fetch_and_sub:
**	...
**	ldaxr	.*
**	sub	.*
**	scvalue	.*
**	stxr	.*
**	cbnz	.*
**	...
*/

/*
** test_intcap_fetch_and_and:
**	...
**	ldaxr	.*
**	and	.*
**	scvalue	.*
**	stxr	.*
**	cbnz	.*
**	...
*/

/*
** test_intcap_fetch_and_nand:
**	...
**	ldaxr	.*
**	.*
**	stxr	.*
**	cbnz	.*
**	...
*/

/*
** test_intcap_fetch_and_or:
**	...
**	ldaxr	.*
**	orr	.*
**	scvalue	.*
**	stxr	.*
**	cbnz	.*
**	...
*/

/*
** test_intcap_fetch_and_xor:
**	...
**	ldaxr	.*
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
