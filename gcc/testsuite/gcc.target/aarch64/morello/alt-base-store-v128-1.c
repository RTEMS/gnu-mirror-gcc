/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <arm_neon.h>

#define ALT_BASE
#include "load-store-utils.h"

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_OFFSET (x10, int8x16_t, m257)
STORE_REG_OFFSET (x10, int16x8_t, m256)
STORE_REG_OFFSET (x10, int32x4_t, m255)
STORE_REG_OFFSET (x10, int64x2_t, m1)
STORE_REG_OFFSET (x10, float16x8_t, 1)
STORE_REG_OFFSET (x10, bfloat16x8_t, 247)
STORE_REG_OFFSET (x10, float32x4_t, 248)
STORE_REG_OFFSET (x10, float64x2_t, 249)
STORE_REG_OFFSET (x10, int8x16_t, 256)
STORE_REG_OFFSET (x10, int8x16_t, 511)
STORE_REG_OFFSET (x10, int8x16_t, 512)

STORE_REG_INDEX (x10, int8x16_t, int32_t, 1)
STORE_REG_INDEX (x10, int8x16_t, uint32_t, 1)
STORE_REG_INDEX (x10, int8x16_t, uint64_t, 1)

STORE_REG_INDEX (x10, int8x16_t, int32_t, 2)
STORE_REG_INDEX (x10, int8x16_t, uint32_t, 2)
STORE_REG_INDEX (x10, int8x16_t, uint64_t, 2)

STORE_REG_INDEX (x10, int8x16_t, int32_t, 4)
STORE_REG_INDEX (x10, int8x16_t, uint32_t, 4)
STORE_REG_INDEX (x10, int8x16_t, uint64_t, 4)

STORE_REG_INDEX (x10, int8x16_t, int32_t, 8)
STORE_REG_INDEX (x10, int8x16_t, uint32_t, 8)
STORE_REG_INDEX (x10, int8x16_t, uint64_t, 8)

STORE_REG_INDEX (x10, int8x16_t, int32_t, 16)
STORE_REG_INDEX (x10, int8x16_t, uint32_t, 16)
STORE_REG_INDEX (x10, int8x16_t, uint64_t, 16)

/*
** store_q20_int8x16_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	q20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16_t, m257)

/*
** store_q20_int16x8_t_m256:
**	str	q20, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (q20, int16x8_t, m256)

/*
** store_q20_int32x4_t_m255:
**	str	q20, \[c0, #?-255\]
**	ret
*/
STORE_REG_OFFSET (q20, int32x4_t, m255)

/*
** store_q20_int64x2_t_m1:
**	str	q20, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (q20, int64x2_t, m1)

/*
** store_q20_float16x8_t_1:
**	str	q20, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (q20, float16x8_t, 1)

/*
** store_q20_bfloat16x8_t_247:
**	str	q20, \[c0, #?247\]
**	ret
*/
STORE_REG_OFFSET (q20, bfloat16x8_t, 247)

/*
** store_q20_float32x4_t_248:
**	str	q20, \[c0, #?248\]
**	ret
*/
STORE_REG_OFFSET (q20, float32x4_t, 248)

/*
** store_q20_float64x2_t_249:
**	str	q20, \[c0, #?249\]
**	ret
*/
STORE_REG_OFFSET (q20, float64x2_t, 249)

/*
** store_q20_int8x16_t_256:
**	add	(c[0-9]+), c0, #?256
**	str	q20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (q20, int8x16_t, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (q20, int8x16_t, int32_t, 1)
STORE_REG_INDEX (q20, int8x16_t, uint32_t, 1)
STORE_REG_INDEX (q20, int8x16_t, uint64_t, 1)

STORE_REG_INDEX (q20, int8x16_t, int32_t, 2)
STORE_REG_INDEX (q20, int8x16_t, uint32_t, 2)
STORE_REG_INDEX (q20, int8x16_t, uint64_t, 2)

STORE_REG_INDEX (q20, int8x16_t, int32_t, 4)
STORE_REG_INDEX (q20, int8x16_t, uint32_t, 4)
STORE_REG_INDEX (q20, int8x16_t, uint64_t, 4)

STORE_REG_INDEX (q20, int8x16_t, int32_t, 8)
STORE_REG_INDEX (q20, int8x16_t, uint32_t, 8)
STORE_REG_INDEX (q20, int8x16_t, uint64_t, 8)

STORE_REG_INDEX (q20, int8x16_t, int32_t, 16)
STORE_REG_INDEX (q20, int8x16_t, uint32_t, 16)
STORE_REG_INDEX (q20, int8x16_t, uint64_t, 16)
