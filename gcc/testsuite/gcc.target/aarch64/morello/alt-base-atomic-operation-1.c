/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_OPERATION(TYPE, SIZE, OPERATION)				\
  TYPE									\
  test_##TYPE##_fetch_##OPERATION (TYPE *__capability ptr, TYPE val)	\
  {									\
    return __atomic_fetch_##OPERATION##_##SIZE##_c (ptr, val,		\
						    __ATOMIC_RELAXED);	\
  }									\
									\
  TYPE									\
  test_##TYPE##_##OPERATION##_fetch (TYPE *__capability ptr, TYPE val)	\
  {									\
    return __atomic_##OPERATION##_fetch_##SIZE##_c (ptr, val,		\
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
**	mov	w2, #?0
**	b	__atomic_fetch_add_1_c
*/

/*
** test_uint8_t_fetch_sub:
**	mov	w2, #?0
**	b	__atomic_fetch_sub_1_c
*/

/*
** test_uint8_t_fetch_and:
**	mov	w2, #?0
**	b	__atomic_fetch_and_1_c
*/

/*
** test_uint8_t_fetch_nand:
**	mov	w2, #?0
**	b	__atomic_fetch_nand_1_c
*/

/*
** test_uint8_t_fetch_or:
**	mov	w2, #?0
**	b	__atomic_fetch_or_1_c
*/

/*
** test_uint8_t_fetch_xor:
**	mov	w2, #?0
**	b	__atomic_fetch_xor_1_c
*/

/*
** test_uint8_t_add_fetch:
**	...
**	bl	__atomic_fetch_add_1_c
**	...
**	add	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/

/*
** test_uint8_t_sub_fetch:
**	...
**	bl	__atomic_fetch_sub_1_c
**	...
**	sub	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/

/*
** test_uint8_t_and_fetch:
**	...
**	bl	__atomic_fetch_and_1_c
**	...
**	and	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/

/*
** test_uint8_t_nand_fetch:
**	...
**	bl	__atomic_fetch_nand_1_c
**	...
**	and	(w[0-9]+), (w[0-9]+, w0|w0, w[0-9]+)
**	mvn	w0, \1
**	...
*/

/*
** test_uint8_t_or_fetch:
**	...
**	bl	__atomic_fetch_or_1_c
**	...
**	orr	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/

/*
** test_uint8_t_xor_fetch:
**	...
**	bl	__atomic_fetch_xor_1_c
**	...
**	eor	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/
TEST_SIZE (uint8_t, 1)

/*
** test_uint16_t_fetch_add:
**	mov	w2, #?0
**	b	__atomic_fetch_add_2_c
*/

/*
** test_uint16_t_fetch_sub:
**	mov	w2, #?0
**	b	__atomic_fetch_sub_2_c
*/

/*
** test_uint16_t_fetch_and:
**	mov	w2, #?0
**	b	__atomic_fetch_and_2_c
*/

/*
** test_uint16_t_fetch_nand:
**	mov	w2, #?0
**	b	__atomic_fetch_nand_2_c
*/

/*
** test_uint16_t_fetch_or:
**	mov	w2, #?0
**	b	__atomic_fetch_or_2_c
*/

/*
** test_uint16_t_fetch_xor:
**	mov	w2, #?0
**	b	__atomic_fetch_xor_2_c
*/

/*
** test_uint16_t_add_fetch:
**	...
**	bl	__atomic_fetch_add_2_c
**	...
**	add	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/

/*
** test_uint16_t_sub_fetch:
**	...
**	bl	__atomic_fetch_sub_2_c
**	...
**	sub	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/

/*
** test_uint16_t_and_fetch:
**	...
**	bl	__atomic_fetch_and_2_c
**	...
**	and	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/

/*
** test_uint16_t_nand_fetch:
**	...
**	bl	__atomic_fetch_nand_2_c
**	...
**	and	(w[0-9]+), (w[0-9]+, w0|w0, w[0-9]+)
**	mvn	w0, \1
**	...
*/

/*
** test_uint16_t_or_fetch:
**	...
**	bl	__atomic_fetch_or_2_c
**	...
**	orr	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/

/*
** test_uint16_t_xor_fetch:
**	...
**	bl	__atomic_fetch_xor_2_c
**	...
**	eor	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/
TEST_SIZE (uint16_t, 2)

/*
** test_uint32_t_fetch_add:
**	mov	w2, #?0
**	b	__atomic_fetch_add_4_c
*/

/*
** test_uint32_t_fetch_sub:
**	mov	w2, #?0
**	b	__atomic_fetch_sub_4_c
*/

/*
** test_uint32_t_fetch_and:
**	mov	w2, #?0
**	b	__atomic_fetch_and_4_c
*/

/*
** test_uint32_t_fetch_nand:
**	mov	w2, #?0
**	b	__atomic_fetch_nand_4_c
*/

/*
** test_uint32_t_fetch_or:
**	mov	w2, #?0
**	b	__atomic_fetch_or_4_c
*/

/*
** test_uint32_t_fetch_xor:
**	mov	w2, #?0
**	b	__atomic_fetch_xor_4_c
*/

/*
** test_uint32_t_add_fetch:
**	...
**	bl	__atomic_fetch_add_4_c
**	...
**	add	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/

/*
** test_uint32_t_sub_fetch:
**	...
**	bl	__atomic_fetch_sub_4_c
**	...
**	sub	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/

/*
** test_uint32_t_and_fetch:
**	...
**	bl	__atomic_fetch_and_4_c
**	...
**	and	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/

/*
** test_uint32_t_nand_fetch:
**	...
**	bl	__atomic_fetch_nand_4_c
**	...
**	and	(w[0-9]+), (w[0-9]+, w0|w0, w[0-9]+)
**	mvn	w0, \1
**	...
*/

/*
** test_uint32_t_or_fetch:
**	...
**	bl	__atomic_fetch_or_4_c
**	...
**	orr	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/

/*
** test_uint32_t_xor_fetch:
**	...
**	bl	__atomic_fetch_xor_4_c
**	...
**	eor	w0, (w[0-9]+, w0|w0, w[0-9]+)
**	...
*/
TEST_SIZE (uint32_t, 4)

/*
** test_uint64_t_fetch_add:
**	mov	w2, #?0
**	b	__atomic_fetch_add_8_c
*/

/*
** test_uint64_t_fetch_sub:
**	mov	w2, #?0
**	b	__atomic_fetch_sub_8_c
*/

/*
** test_uint64_t_fetch_and:
**	mov	w2, #?0
**	b	__atomic_fetch_and_8_c
*/

/*
** test_uint64_t_fetch_nand:
**	mov	w2, #?0
**	b	__atomic_fetch_nand_8_c
*/

/*
** test_uint64_t_fetch_or:
**	mov	w2, #?0
**	b	__atomic_fetch_or_8_c
*/

/*
** test_uint64_t_fetch_xor:
**	mov	w2, #?0
**	b	__atomic_fetch_xor_8_c
*/

/*
** test_uint64_t_add_fetch:
**	...
**	bl	__atomic_fetch_add_8_c
**	...
**	add	x0, (x[0-9]+, x0|x0, x[0-9]+)
**	...
*/

/*
** test_uint64_t_sub_fetch:
**	...
**	bl	__atomic_fetch_sub_8_c
**	...
**	sub	x0, (x[0-9]+, x0|x0, x[0-9]+)
**	...
*/

/*
** test_uint64_t_and_fetch:
**	...
**	bl	__atomic_fetch_and_8_c
**	...
**	and	x0, (x[0-9]+, x0|x0, x[0-9]+)
**	...
*/

/*
** test_uint64_t_nand_fetch:
**	...
**	bl	__atomic_fetch_nand_8_c
**	...
**	and	(x[0-9]+), (x[0-9]+, x0|x0, x[0-9]+)
**	mvn	x0, \1
**	...
*/

/*
** test_uint64_t_or_fetch:
**	...
**	bl	__atomic_fetch_or_8_c
**	...
**	orr	x0, (x[0-9]+, x0|x0, x[0-9]+)
**	...
*/

/*
** test_uint64_t_xor_fetch:
**	...
**	bl	__atomic_fetch_xor_8_c
**	...
**	eor	x0, (x[0-9]+, x0|x0, x[0-9]+)
**	...
*/
TEST_SIZE (uint64_t, 8)

TEST_SIZE (uint128, 16)

/*
** test_intcap_fetch_add:
**	mov	w2, #?0
**	b	__atomic_fetch_add_capability_c
*/

/*
** test_intcap_fetch_sub:
**	mov	w2, #?0
**	b	__atomic_fetch_sub_capability_c
*/

/*
** test_intcap_fetch_and:
**	mov	w2, #?0
**	b	__atomic_fetch_and_capability_c
*/

/*
** test_intcap_fetch_nand:
**	mov	w2, #?0
**	b	__atomic_fetch_nand_capability_c
*/

/*
** test_intcap_fetch_or:
**	mov	w2, #?0
**	b	__atomic_fetch_or_capability_c
*/

/*
** test_intcap_fetch_xor:
**	mov	w2, #?0
**	b	__atomic_fetch_xor_capability_c
*/

/*
** test_intcap_add_fetch:
**	...
**	bl	__atomic_fetch_add_capability_c
**	...
**	add	c0, c0, x[0-9]+
**	...
*/

/*
** test_intcap_sub_fetch:
**	...
**	bl	__atomic_fetch_sub_capability_c
**	...
**	sub	(x[0-9]+), (x[0-9]+, x0|x0, x[0-9]+)
**	scvalue	c0, c0, \1
**	...
*/

/*
** test_intcap_and_fetch:
**	...
**	bl	__atomic_fetch_and_capability_c
**	...
**	and	(x[0-9]+), (x[0-9]+, x0|x0, x[0-9]+)
**	scvalue	c0, c0, \1
**	...
*/

/*
** test_intcap_nand_fetch:
**	...
**	bl	__atomic_fetch_nand_capability_c
**	...
**	and	(x[0-9]+), (x[0-9]+, x0|x0, x[0-9]+)
**	mvn	(x[0-9]+), \1
**	scvalue	c0, c0, \3
**	...
*/

/*
** test_intcap_or_fetch:
**	...
**	bl	__atomic_fetch_or_capability_c
**	...
**	orr	(x[0-9]+), (x[0-9]+, x0|x0, x[0-9]+)
**	scvalue	c0, c0, \1
**	...
*/

/*
** test_intcap_xor_fetch:
**	...
**	bl	__atomic_fetch_xor_capability_c
**	...
**	eor	(x[0-9]+), (x[0-9]+, x0|x0, x[0-9]+)
**	scvalue	c0, c0, \1
**	...
*/
TEST_SIZE (intcap, capability)
