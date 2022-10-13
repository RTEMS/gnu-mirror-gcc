/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

#include <arm_neon.h>

/*
** load_q20_int8x16x2_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	q20, \[\1\]
**	ldr	q21, \[\1, #?16\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x2_t, m257)

/*
** load_q20_int8x16x2_t_m256:
**	ldr	q20, \[c0, #?-256\]
**	ldr	q21, \[c0, #?-240\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x2_t, m256)

/*
** load_q20_int8x16x2_t_m255:
**	ldr	q20, \[c0, #?-255\]
**	ldr	q21, \[c0, #?-239\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x2_t, m255)

/*
** load_q20_int8x16x2_t_m1:
**	ldr	q20, \[c0, #?-1\]
**	ldr	q21, \[c0, #?15\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x2_t, m1)

/*
** load_q20_int8x16x2_t_1:
**	ldr	q20, \[c0, #?1\]
**	ldr	q21, \[c0, #?17\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x2_t, 1)

/*
** load_q20_int8x16x2_t_239:
**	ldr	q20, \[c0, #?239\]
**	ldr	q21, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x2_t, 239)

/*
** load_q20_int8x16x2_t_240:
**	add	(c[0-9]+), c0, #?240
**	ldr	q20, \[\1\]
**	ldr	q21, \[\1, #?16\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x2_t, 240)

/*
** load_q20_int8x16x2_t_241:
**	add	(c[0-9]+), c0, #?241
**	ldr	q20, \[\1\]
**	ldr	q21, \[\1, #?16\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x2_t, 241)

/*
** load_q20_int8x16x2_t_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	q20, \[\1\]
**	ldr	q21, \[\1, #?16\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x2_t, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (q20, int8x16x2_t, int32_t, 1)
LOAD_REG_INDEX (q20, int8x16x2_t, uint32_t, 1)
LOAD_REG_INDEX (q20, int8x16x2_t, uint64_t, 1)

LOAD_REG_INDEX (q20, int8x16x2_t, int32_t, 2)
LOAD_REG_INDEX (q20, int8x16x2_t, uint32_t, 2)
LOAD_REG_INDEX (q20, int8x16x2_t, uint64_t, 2)

LOAD_REG_INDEX (q20, int8x16x2_t, int32_t, 4)
LOAD_REG_INDEX (q20, int8x16x2_t, uint32_t, 4)
LOAD_REG_INDEX (q20, int8x16x2_t, uint64_t, 4)

LOAD_REG_INDEX (q20, int8x16x2_t, int32_t, 8)
LOAD_REG_INDEX (q20, int8x16x2_t, uint32_t, 8)
LOAD_REG_INDEX (q20, int8x16x2_t, uint64_t, 8)

LOAD_REG_INDEX (q20, int8x16x2_t, int32_t, 16)
LOAD_REG_INDEX (q20, int8x16x2_t, uint32_t, 16)
LOAD_REG_INDEX (q20, int8x16x2_t, uint64_t, 16)
