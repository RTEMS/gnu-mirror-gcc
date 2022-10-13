/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

#include <arm_neon.h>

/*
** store_q20_int8x16x2_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	q20, \[\1\]
**	str	q21, \[\1, #?16\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x2_t, m257)

/*
** store_q20_int8x16x2_t_m256:
**	str	q20, \[c0, #?-256\]
**	str	q21, \[c0, #?-240\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x2_t, m256)

/*
** store_q20_int8x16x2_t_m255:
**	str	q20, \[c0, #?-255\]
**	str	q21, \[c0, #?-239\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x2_t, m255)

/*
** store_q20_int8x16x2_t_m1:
**	str	q20, \[c0, #?-1\]
**	str	q21, \[c0, #?15\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x2_t, m1)

/*
** store_q20_int8x16x2_t_1:
**	str	q20, \[c0, #?1\]
**	str	q21, \[c0, #?17\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x2_t, 1)

/*
** store_q20_int8x16x2_t_239:
**	str	q20, \[c0, #?239\]
**	str	q21, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x2_t, 239)

/*
** store_q20_int8x16x2_t_240:
**	add	(c[0-9]+), c0, #?240
**	str	q20, \[\1\]
**	str	q21, \[\1, #?16\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x2_t, 240)

/*
** store_q20_int8x16x2_t_241:
**	add	(c[0-9]+), c0, #?241
**	str	q20, \[\1\]
**	str	q21, \[\1, #?16\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x2_t, 241)

/*
** store_q20_int8x16x2_t_256:
**	add	(c[0-9]+), c0, #?256
**	str	q20, \[\1\]
**	str	q21, \[\1, #?16\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x2_t, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (q20, int8x16x2_t, int32_t, 1)
STORE_REG_INDEX (q20, int8x16x2_t, uint32_t, 1)
STORE_REG_INDEX (q20, int8x16x2_t, uint64_t, 1)

STORE_REG_INDEX (q20, int8x16x2_t, int32_t, 2)
STORE_REG_INDEX (q20, int8x16x2_t, uint32_t, 2)
STORE_REG_INDEX (q20, int8x16x2_t, uint64_t, 2)

STORE_REG_INDEX (q20, int8x16x2_t, int32_t, 4)
STORE_REG_INDEX (q20, int8x16x2_t, uint32_t, 4)
STORE_REG_INDEX (q20, int8x16x2_t, uint64_t, 4)

STORE_REG_INDEX (q20, int8x16x2_t, int32_t, 8)
STORE_REG_INDEX (q20, int8x16x2_t, uint32_t, 8)
STORE_REG_INDEX (q20, int8x16x2_t, uint64_t, 8)

STORE_REG_INDEX (q20, int8x16x2_t, int32_t, 16)
STORE_REG_INDEX (q20, int8x16x2_t, uint32_t, 16)
STORE_REG_INDEX (q20, int8x16x2_t, uint64_t, 16)
