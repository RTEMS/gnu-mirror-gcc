/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

#include <arm_neon.h>

/*
** store_q20_int8x16x4_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	q20, \[\1\]
**	str	q21, \[\1, #?16\]
**	str	q22, \[\1, #?32\]
**	str	q23, \[\1, #?48\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x4_t, m257)

/*
** store_q20_int8x16x4_t_m256:
**	str	q20, \[c0, #?-256\]
**	str	q21, \[c0, #?-240\]
**	str	q22, \[c0, #?-224\]
**	str	q23, \[c0, #?-208\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x4_t, m256)

/*
** store_q20_int8x16x4_t_m255:
**	str	q20, \[c0, #?-255\]
**	str	q21, \[c0, #?-239\]
**	str	q22, \[c0, #?-223\]
**	str	q23, \[c0, #?-207\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x4_t, m255)

/*
** store_q20_int8x16x4_t_m1:
**	str	q20, \[c0, #?-1\]
**	str	q21, \[c0, #?15\]
**	str	q22, \[c0, #?31\]
**	str	q23, \[c0, #?47\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x4_t, m1)

/*
** store_q20_int8x16x4_t_1:
**	str	q20, \[c0, #?1\]
**	str	q21, \[c0, #?17\]
**	str	q22, \[c0, #?33\]
**	str	q23, \[c0, #?49\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x4_t, 1)

/*
** store_q20_int8x16x4_t_207:
**	str	q20, \[c0, #?207\]
**	str	q21, \[c0, #?223\]
**	str	q22, \[c0, #?239\]
**	str	q23, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x4_t, 207)

/*
** store_q20_int8x16x4_t_208:
**	add	(c[0-9]+), c0, #?208
**	str	q20, \[\1\]
**	str	q21, \[\1, #?16\]
**	str	q22, \[\1, #?32\]
**	str	q23, \[\1, #?48\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x4_t, 208)

/*
** store_q20_int8x16x4_t_209:
**	add	(c[0-9]+), c0, #?209
**	str	q20, \[\1\]
**	str	q21, \[\1, #?16\]
**	str	q22, \[\1, #?32\]
**	str	q23, \[\1, #?48\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x4_t, 209)

/*
** store_q20_int8x16x4_t_256:
**	add	(c[0-9]+), c0, #?256
**	str	q20, \[\1\]
**	str	q21, \[\1, #?16\]
**	str	q22, \[\1, #?32\]
**	str	q23, \[\1, #?48\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16x4_t, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (q20, int8x16x4_t, int32_t, 1)
STORE_REG_INDEX (q20, int8x16x4_t, uint32_t, 1)
STORE_REG_INDEX (q20, int8x16x4_t, uint64_t, 1)

STORE_REG_INDEX (q20, int8x16x4_t, int32_t, 2)
STORE_REG_INDEX (q20, int8x16x4_t, uint32_t, 2)
STORE_REG_INDEX (q20, int8x16x4_t, uint64_t, 2)

STORE_REG_INDEX (q20, int8x16x4_t, int32_t, 4)
STORE_REG_INDEX (q20, int8x16x4_t, uint32_t, 4)
STORE_REG_INDEX (q20, int8x16x4_t, uint64_t, 4)

STORE_REG_INDEX (q20, int8x16x4_t, int32_t, 8)
STORE_REG_INDEX (q20, int8x16x4_t, uint32_t, 8)
STORE_REG_INDEX (q20, int8x16x4_t, uint64_t, 8)

STORE_REG_INDEX (q20, int8x16x4_t, int32_t, 16)
STORE_REG_INDEX (q20, int8x16x4_t, uint32_t, 16)
STORE_REG_INDEX (q20, int8x16x4_t, uint64_t, 16)
