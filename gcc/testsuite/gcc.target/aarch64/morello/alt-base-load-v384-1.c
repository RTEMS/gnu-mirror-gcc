/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

#include <arm_neon.h>

/*
** load_q20_int8x16x3_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	q20, \[\1\]
**	ldr	q21, \[\1, #?16\]
**	ldr	q22, \[\1, #?32\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x3_t, m257)

/*
** load_q20_int8x16x3_t_m256:
**	ldr	q20, \[c0, #?-256\]
**	ldr	q21, \[c0, #?-240\]
**	ldr	q22, \[c0, #?-224\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x3_t, m256)

/*
** load_q20_int8x16x3_t_m255:
**	ldr	q20, \[c0, #?-255\]
**	ldr	q21, \[c0, #?-239\]
**	ldr	q22, \[c0, #?-223\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x3_t, m255)

/*
** load_q20_int8x16x3_t_m1:
**	ldr	q20, \[c0, #?-1\]
**	ldr	q21, \[c0, #?15\]
**	ldr	q22, \[c0, #?31\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x3_t, m1)

/*
** load_q20_int8x16x3_t_1:
**	ldr	q20, \[c0, #?1\]
**	ldr	q21, \[c0, #?17\]
**	ldr	q22, \[c0, #?33\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x3_t, 1)

/*
** load_q20_int8x16x3_t_223:
**	ldr	q20, \[c0, #?223\]
**	ldr	q21, \[c0, #?239\]
**	ldr	q22, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x3_t, 223)

/*
** load_q20_int8x16x3_t_224:
**	add	(c[0-9]+), c0, #?224
**	ldr	q20, \[\1\]
**	ldr	q21, \[\1, #?16\]
**	ldr	q22, \[\1, #?32\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x3_t, 224)

/*
** load_q20_int8x16x3_t_225:
**	add	(c[0-9]+), c0, #?225
**	ldr	q20, \[\1\]
**	ldr	q21, \[\1, #?16\]
**	ldr	q22, \[\1, #?32\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x3_t, 225)

/*
** load_q20_int8x16x3_t_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	q20, \[\1\]
**	ldr	q21, \[\1, #?16\]
**	ldr	q22, \[\1, #?32\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16x3_t, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (q20, int8x16x3_t, int32_t, 1)
LOAD_REG_INDEX (q20, int8x16x3_t, uint32_t, 1)
LOAD_REG_INDEX (q20, int8x16x3_t, uint64_t, 1)

LOAD_REG_INDEX (q20, int8x16x3_t, int32_t, 2)
LOAD_REG_INDEX (q20, int8x16x3_t, uint32_t, 2)
LOAD_REG_INDEX (q20, int8x16x3_t, uint64_t, 2)

LOAD_REG_INDEX (q20, int8x16x3_t, int32_t, 4)
LOAD_REG_INDEX (q20, int8x16x3_t, uint32_t, 4)
LOAD_REG_INDEX (q20, int8x16x3_t, uint64_t, 4)

LOAD_REG_INDEX (q20, int8x16x3_t, int32_t, 8)
LOAD_REG_INDEX (q20, int8x16x3_t, uint32_t, 8)
LOAD_REG_INDEX (q20, int8x16x3_t, uint64_t, 8)

LOAD_REG_INDEX (q20, int8x16x3_t, int32_t, 16)
LOAD_REG_INDEX (q20, int8x16x3_t, uint32_t, 16)
LOAD_REG_INDEX (q20, int8x16x3_t, uint64_t, 16)
