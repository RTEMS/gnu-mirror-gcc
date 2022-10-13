/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

#include <arm_neon.h>

/*
** store_q20_int8x16x3_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	q20, \[\1\]
**	str	q21, \[\1, #?16\]
**	str	q22, \[\1, #?32\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x3_t, m257)

/*
** store_q20_int8x16x3_t_m256:
**	str	q20, \[c0, #?-256\]
**	str	q21, \[c0, #?-240\]
**	str	q22, \[c0, #?-224\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x3_t, m256)

/*
** store_q20_int8x16x3_t_m255:
**	str	q20, \[c0, #?-255\]
**	str	q21, \[c0, #?-239\]
**	str	q22, \[c0, #?-223\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x3_t, m255)

/*
** store_q20_int8x16x3_t_m1:
**	str	q20, \[c0, #?-1\]
**	str	q21, \[c0, #?15\]
**	str	q22, \[c0, #?31\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x3_t, m1)

/*
** store_q20_int8x16x3_t_1:
**	str	q20, \[c0, #?1\]
**	str	q21, \[c0, #?17\]
**	str	q22, \[c0, #?33\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x3_t, 1)

/*
** store_q20_int8x16x3_t_223:
**	str	q20, \[c0, #?223\]
**	str	q21, \[c0, #?239\]
**	str	q22, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x3_t, 223)

/*
** store_q20_int8x16x3_t_224:
**	add	(c[0-9]+), c0, #?224
**	str	q20, \[\1\]
**	str	q21, \[\1, #?16\]
**	str	q22, \[\1, #?32\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x3_t, 224)

/*
** store_q20_int8x16x3_t_225:
**	add	(c[0-9]+), c0, #?225
**	str	q20, \[\1\]
**	str	q21, \[\1, #?16\]
**	str	q22, \[\1, #?32\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x3_t, 225)

/*
** store_q20_int8x16x3_t_256:
**	add	(c[0-9]+), c0, #?256
**	str	q20, \[\1\]
**	str	q21, \[\1, #?16\]
**	str	q22, \[\1, #?32\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x3_t, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (q20, int8x16x3_t, int32_t, 1)
STORE_REG_INDEX (q20, int8x16x3_t, uint32_t, 1)
STORE_REG_INDEX (q20, int8x16x3_t, uint64_t, 1)

STORE_REG_INDEX (q20, int8x16x3_t, int32_t, 2)
STORE_REG_INDEX (q20, int8x16x3_t, uint32_t, 2)
STORE_REG_INDEX (q20, int8x16x3_t, uint64_t, 2)

STORE_REG_INDEX (q20, int8x16x3_t, int32_t, 4)
STORE_REG_INDEX (q20, int8x16x3_t, uint32_t, 4)
STORE_REG_INDEX (q20, int8x16x3_t, uint64_t, 4)

STORE_REG_INDEX (q20, int8x16x3_t, int32_t, 8)
STORE_REG_INDEX (q20, int8x16x3_t, uint32_t, 8)
STORE_REG_INDEX (q20, int8x16x3_t, uint64_t, 8)

STORE_REG_INDEX (q20, int8x16x3_t, int32_t, 16)
STORE_REG_INDEX (q20, int8x16x3_t, uint32_t, 16)
STORE_REG_INDEX (q20, int8x16x3_t, uint64_t, 16)
