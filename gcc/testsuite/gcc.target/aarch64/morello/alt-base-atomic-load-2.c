/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps -Wno-cheri-implicit-pointer-conversion-from-cap -Wno-cheri-explicit-pointer-conversion-from-cap" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_SIZE(TYPE, SIZE)					\
  TYPE								\
  test_##TYPE (TYPE *__capability ptr)				\
  {								\
    return __atomic_load_n (ptr, __ATOMIC_ACQUIRE);		\
  }								\
								\
  TYPE								\
  test_##TYPE##_offset (TYPE *__capability ptr)			\
  {								\
    return __atomic_load_n (ptr + 1, __ATOMIC_ACQUIRE);		\
  }								\
								\
  TYPE								\
  test_##TYPE##_index (TYPE *__capability ptr, int index)	\
  {								\
    return __atomic_load_n (ptr + index, __ATOMIC_ACQUIRE);	\
  }								\
								\
  TYPE								\
  test_##TYPE##_convert (TYPE *__capability ptr, int index)	\
  {								\
    return __atomic_load_n ((TYPE *) ptr, __ATOMIC_ACQUIRE);	\
  }								\
								\
  TYPE								\
  test_##TYPE##_relaxed (TYPE *__capability ptr, int index)	\
  {								\
    return __atomic_load_n (ptr, __ATOMIC_RELAXED);		\
  }								\
								\
  TYPE								\
  test_##TYPE##_consume (TYPE *__capability ptr, int index)	\
  {								\
    return __atomic_load_n (ptr, __ATOMIC_CONSUME);		\
  }								\
								\
  TYPE								\
  test_##TYPE##_seq_cst (TYPE *__capability ptr, int index)	\
  {								\
    return __atomic_load_n (ptr, __ATOMIC_SEQ_CST);		\
  }

/*
** test_uint8_t:
**	ldarb	w0, \[c0\]
**	ret
*/

/*
** test_uint8_t_offset:
**	add	(c[0-9]+), c0, #?1
**	ldarb	w0, \[\1\]
**	ret
*/

/*
** test_uint8_t_index:
**	add	(c[0-9]+), c0, w1, sxtw
**	ldarb	w0, \[\1\]
**	ret
*/

/*
** test_uint8_t_convert:
**	ldarb	w0, \[x0\]
**	ret
*/

/*
** test_uint8_t_relaxed:
**	ldrb	w0, \[c0\]
**	ret
*/

/*
** test_uint8_t_consume:
**	ldarb	w0, \[c0\]
**	ret
*/

/*
** test_uint8_t_seq_cst:
**	ldarb	w0, \[c0\]
**	ret
*/
TEST_SIZE (uint8_t, 1)

/*
** test_uint16_t:
**	ldrh	w0, \[c0\]
**	dmb	ishld
**	ret
*/

/*
** test_uint16_t_offset:
**	ldrh	w0, \[c0, #?2\]
**	dmb	ishld
**	ret
*/

/* test_uint16_t_index not matched.  */

/*
** test_uint16_t_convert:
**	ldarh	w0, \[x0\]
**	ret
*/

/*
** test_uint16_t_relaxed:
**	ldrh	w0, \[c0\]
**	ret
*/

/*
** test_uint16_t_consume:
**	ldrh	w0, \[c0\]
**	dmb	ishld
**	ret
*/

/*
** test_uint16_t_seq_cst:
**	dmb	ish
**	ldrh	w0, \[c0\]
**	dmb	ish
**	ret
*/
TEST_SIZE (uint16_t, 2)

/*
** test_uint32_t:
**	ldar	w0, \[c0\]
**	ret
*/

/*
** test_uint32_t_offset:
**	add	(c[0-9]+), c0, #?4
**	ldar	w0, \[\1\]
**	ret
*/

/*
** test_uint32_t_index:
**	add	(c[0-9]+), c0, w1, sxtw #?2
**	ldar	w0, \[\1\]
**	ret
*/

/*
** test_uint32_t_convert:
**	ldar	w0, \[x0\]
**	ret
*/

/*
** test_uint32_t_relaxed:
**	ldr	w0, \[c0\]
**	ret
*/

/*
** test_uint32_t_consume:
**	ldar	w0, \[c0\]
**	ret
*/

/*
** test_uint32_t_seq_cst:
**	ldar	w0, \[c0\]
**	ret
*/
TEST_SIZE (uint32_t, 4)

/*
** test_uint64_t:
**	ldr	x0, \[c0\]
**	dmb	ishld
**	ret
*/

/*
** test_uint64_t_offset:
**	ldr	x0, \[c0, #?8\]
**	dmb	ishld
**	ret
*/

/* test_uint64_t_index not matched.  */

/*
** test_uint64_t_convert:
**	ldar	x0, \[x0\]
**	ret
*/

/*
** test_uint64_t_relaxed:
**	ldr	x0, \[c0\]
**	ret
*/

/*
** test_uint64_t_consume:
**	ldr	x0, \[c0\]
**	dmb	ishld
**	ret
*/

/*
** test_uint64_t_seq_cst:
**	dmb	ish
**	ldr	x0, \[c0\]
**	dmb	ish
**	ret
*/
TEST_SIZE (uint64_t, 8)

/*
** test_uint128:
**	mov	w1, #?2
**	b	__atomic_load_16_c
*/

/*
** test_uint128_convert:
**	mov	w1, #?2
**	b	__atomic_load_16
*/

/* Others test_uint128_t not matched.  */
TEST_SIZE (uint128, 16)

/*
** test_intcap:
**	ldar	c0, \[c0\]
**	ret
*/

/*
** test_intcap_offset:
**	add	(c[0-9]+), c0, #?16
**	ldar	c0, \[\1\]
**	ret
*/

/*
** test_intcap_index:
**	add	(c[0-9]+), c0, w1, sxtw #?4
**	ldar	c0, \[\1\]
**	ret
*/

/*
** test_intcap_convert:
**	ldar	c0, \[x0\]
**	ret
*/

/*
** test_intcap_relaxed:
**	ldr	c0, \[c0\]
**	ret
*/

/*
** test_intcap_consume:
**	ldar	c0, \[c0\]
**	ret
*/

/*
** test_intcap_seq_cst:
**	ldar	c0, \[c0\]
**	ret
*/
TEST_SIZE (intcap, capability)
