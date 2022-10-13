/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps -Wno-cheri-implicit-pointer-conversion-from-cap -Wno-cheri-explicit-pointer-conversion-from-cap" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_SIZE(TYPE, SIZE)						\
  void									\
  test_##TYPE (TYPE *__capability ptr, TYPE data)			\
  {									\
    __atomic_store_##SIZE##_c (ptr, data, __ATOMIC_RELEASE);		\
  }									\
									\
  TYPE									\
  test_##TYPE##_offset (TYPE *__capability ptr, TYPE data)		\
  {									\
    __atomic_store_##SIZE##_c (ptr + 1, data, __ATOMIC_RELEASE);	\
  }									\
									\
  TYPE									\
  test_##TYPE##_index (TYPE *__capability ptr, TYPE data, int index)	\
  {									\
    __atomic_store_##SIZE##_c (ptr + index, data, __ATOMIC_RELEASE);	\
  }									\
									\
  TYPE									\
  test_##TYPE##_convert (TYPE *__capability ptr, TYPE data, int index)	\
  {									\
    __atomic_store_##SIZE ((TYPE *) ptr, data, __ATOMIC_RELEASE);	\
  }									\
									\
  TYPE									\
  test_##TYPE##_relaxed (TYPE *__capability ptr, TYPE data, int index)	\
  {									\
    __atomic_store_##SIZE##_c (ptr, data, __ATOMIC_RELAXED);		\
  }									\
									\
  TYPE									\
  test_##TYPE##_seq_cst (TYPE *__capability ptr, TYPE data, int index)	\
  {									\
    __atomic_store_##SIZE##_c (ptr, data, __ATOMIC_SEQ_CST);		\
  }

/*
** test_uint8_t:
**	...
**	stlrb	w1, \[c0\]
**	ret
*/

/*
** test_uint8_t_offset:
**	...
**	add	(c[0-9]+), c0, #?1
**	...
**	stlrb	w1, \[\1\]
**	ret
*/

/*
** test_uint8_t_index:
**	...
**	add	(c[0-9]+), c0, w2, sxtw
**	...
**	stlrb	w1, \[\1\]
**	ret
*/

/*
** test_uint8_t_convert:
**	...
**	stlrb	w1, \[x0\]
**	ret
*/

/*
** test_uint8_t_relaxed:
**	...
**	strb	w1, \[c0\]
**	ret
*/

/*
** test_uint8_t_seq_cst:
**	...
**	stlrb	w1, \[c0\]
**	ret
*/
TEST_SIZE (uint8_t, 1)

/*
** test_uint16_t:
**	...
**	dmb	ish
**	...
**	strh	w1, \[c0\]
**	ret
*/

/* test_uint16_t offset and test_uint16_t_index not matched.  */

/*
** test_uint16_t_convert:
**	...
**	stlrh	w1, \[x0\]
**	ret
*/

/*
** test_uint16_t_relaxed:
**	...
**	strh	w1, \[c0\]
**	ret
*/

/*
** test_uint16_t_seq_cst:
**	...
**	dmb	ish
**	...
**	dmb	ish
**	ret
*/
TEST_SIZE (uint16_t, 2)

/*
** test_uint32_t:
**	stlr	w1, \[c0\]
**	ret
*/

/*
** test_uint32_t_offset:
**	add	(c[0-9]+), c0, #?4
**	stlr	w1, \[\1\]
**	ret
*/

/*
** test_uint32_t_index:
**	add	(c[0-9]+), c0, w2, sxtw #?2
**	stlr	w1, \[\1\]
**	ret
*/

/*
** test_uint32_t_convert:
**	stlr	w1, \[x0\]
**	ret
*/

/*
** test_uint32_t_relaxed:
**	str	w1, \[c0\]
**	ret
*/

/*
** test_uint32_t_seq_cst:
**	stlr	w1, \[c0\]
**	ret
*/
TEST_SIZE (uint32_t, 4)

/*
** test_uint64_t:
**	dmb	ish
**	str	x1, \[c0\]
**	ret
*/

/* test_uint64_t_offset and test_uint64_t_index not matched.  */

/*
** test_uint64_t_convert:
**	stlr	x1, \[x0\]
**	ret
*/

/*
** test_uint64_t_relaxed:
**	str	x1, \[c0\]
**	ret
*/

/*
** test_uint64_t_seq_cst:
**	dmb	ish
**	str	x1, \[c0\]
**	dmb	ish
**	ret
*/
TEST_SIZE (uint64_t, 8)

/*
** test_uint128:
**	mov	w4, #?3
**	b	__atomic_store_16_c
*/

/*
** test_uint128_convert:
**	...
**	bl?	__atomic_store_16
**	...
*/

/* Others test_uint128_t not matched.  */
TEST_SIZE (uint128, 16)

/*
** test_intcap:
**	stlr	c1, \[c0\]
**	ret
*/

/*
** test_intcap_offset:
**	add	(c[0-9]+), c0, #?16
**	stlr	c1, \[\1\]
**	ret
*/

/*
** test_intcap_index:
**	add	(c[0-9]+), c0, w2, sxtw #?4
**	stlr	c1, \[\1\]
**	ret
*/

/*
** test_intcap_convert:
**	stlr	c1, \[x0\]
**	ret
*/

/*
** test_intcap_relaxed:
**	str	c1, \[c0\]
**	ret
*/

/*
** test_intcap_seq_cst:
**	stlr	c1, \[c0\]
**	ret
*/
TEST_SIZE (intcap, capability)
