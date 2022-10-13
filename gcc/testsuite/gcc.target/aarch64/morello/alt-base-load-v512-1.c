/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

#include <arm_neon.h>

/*
** load_q20_int8x16x4_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	q20, \[\1\]
**	ldr	q21, \[\1, #?16\]
**	ldr	q22, \[\1, #?32\]
**	ldr	q23, \[\1, #?48\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x4_t, m257)

/*
** load_q20_int8x16x4_t_m256:
**	ldr	q20, \[c0, #?-256\]
**	ldr	q21, \[c0, #?-240\]
**	ldr	q22, \[c0, #?-224\]
**	ldr	q23, \[c0, #?-208\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x4_t, m256)

/*
** load_q20_int8x16x4_t_m255:
**	ldr	q20, \[c0, #?-255\]
**	ldr	q21, \[c0, #?-239\]
**	ldr	q22, \[c0, #?-223\]
**	ldr	q23, \[c0, #?-207\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x4_t, m255)

/*
** load_q20_int8x16x4_t_m1:
**	ldr	q20, \[c0, #?-1\]
**	ldr	q21, \[c0, #?15\]
**	ldr	q22, \[c0, #?31\]
**	ldr	q23, \[c0, #?47\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x4_t, m1)

/*
** load_q20_int8x16x4_t_1:
**	ldr	q20, \[c0, #?1\]
**	ldr	q21, \[c0, #?17\]
**	ldr	q22, \[c0, #?33\]
**	ldr	q23, \[c0, #?49\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x4_t, 1)

/*
** load_q20_int8x16x4_t_207:
**	ldr	q20, \[c0, #?207\]
**	ldr	q21, \[c0, #?223\]
**	ldr	q22, \[c0, #?239\]
**	ldr	q23, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x4_t, 207)

/*
** load_q20_int8x16x4_t_208:
**	add	(c[0-9]+), c0, #?208
**	ldr	q20, \[\1\]
**	ldr	q21, \[\1, #?16\]
**	ldr	q22, \[\1, #?32\]
**	ldr	q23, \[\1, #?48\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x4_t, 208)

/*
** load_q20_int8x16x4_t_209:
**	add	(c[0-9]+), c0, #?209
**	ldr	q20, \[\1\]
**	ldr	q21, \[\1, #?16\]
**	ldr	q22, \[\1, #?32\]
**	ldr	q23, \[\1, #?48\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x4_t, 209)

/*
** load_q20_int8x16x4_t_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	q20, \[\1\]
**	ldr	q21, \[\1, #?16\]
**	ldr	q22, \[\1, #?32\]
**	ldr	q23, \[\1, #?48\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x4_t, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (q20, int8x16x4_t, int32_t, 1)
LOAD_REG_INDEX (q20, int8x16x4_t, uint32_t, 1)
LOAD_REG_INDEX (q20, int8x16x4_t, uint64_t, 1)

LOAD_REG_INDEX (q20, int8x16x4_t, int32_t, 2)
LOAD_REG_INDEX (q20, int8x16x4_t, uint32_t, 2)
LOAD_REG_INDEX (q20, int8x16x4_t, uint64_t, 2)

LOAD_REG_INDEX (q20, int8x16x4_t, int32_t, 4)
LOAD_REG_INDEX (q20, int8x16x4_t, uint32_t, 4)
LOAD_REG_INDEX (q20, int8x16x4_t, uint64_t, 4)

LOAD_REG_INDEX (q20, int8x16x4_t, int32_t, 8)
LOAD_REG_INDEX (q20, int8x16x4_t, uint32_t, 8)
LOAD_REG_INDEX (q20, int8x16x4_t, uint64_t, 8)

LOAD_REG_INDEX (q20, int8x16x4_t, int32_t, 16)
LOAD_REG_INDEX (q20, int8x16x4_t, uint32_t, 16)
LOAD_REG_INDEX (q20, int8x16x4_t, uint64_t, 16)
