/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <arm_neon.h>

#define ALT_BASE
#include "load-store-utils.h"

/* Check for valid asm, but don't mandate a parint8x16_tcular sequence.  */
LOAD_REG_OFFSET (x10, int8x16_t, m257)
LOAD_REG_OFFSET (x10, int16x8_t, m256)
LOAD_REG_OFFSET (x10, int32x2_t, m255)
LOAD_REG_OFFSET (x10, int64x2_t, m1)
LOAD_REG_OFFSET (x10, float16x4_t, 1)
LOAD_REG_OFFSET (x10, bfloat16x4_t, 247)
LOAD_REG_OFFSET (x10, float32x4_t, 248)
LOAD_REG_OFFSET (x10, float64x2_t, 249)
LOAD_REG_OFFSET (x10, int8x16_t, 256)
LOAD_REG_OFFSET (x10, int8x16_t, 511)
LOAD_REG_OFFSET (x10, int8x16_t, 512)

LOAD_REG_INDEX (x10, int8x16_t, int32_t, 1)
LOAD_REG_INDEX (x10, int8x16_t, uint32_t, 1)
LOAD_REG_INDEX (x10, int8x16_t, uint64_t, 1)

LOAD_REG_INDEX (x10, int8x16_t, int32_t, 2)
LOAD_REG_INDEX (x10, int8x16_t, uint32_t, 2)
LOAD_REG_INDEX (x10, int8x16_t, uint64_t, 2)

LOAD_REG_INDEX (x10, int8x16_t, int32_t, 4)
LOAD_REG_INDEX (x10, int8x16_t, uint32_t, 4)
LOAD_REG_INDEX (x10, int8x16_t, uint64_t, 4)

LOAD_REG_INDEX (x10, int8x16_t, int32_t, 8)
LOAD_REG_INDEX (x10, int8x16_t, uint32_t, 8)
LOAD_REG_INDEX (x10, int8x16_t, uint64_t, 8)

LOAD_REG_INDEX (x10, int8x16_t, int32_t, 16)
LOAD_REG_INDEX (x10, int8x16_t, uint32_t, 16)
LOAD_REG_INDEX (x10, int8x16_t, uint64_t, 16)

/*
** load_q20_int8x16_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	q20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16_t, m257)

/*
** load_q20_int16x8_t_m256:
**	ldr	q20, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (q20, int16x8_t, m256)

/*
** load_q20_int32x4_t_m255:
**	ldr	q20, \[c0, #?-255\]
**	ret
*/
LOAD_REG_OFFSET (q20, int32x4_t, m255)

/*
** load_q20_int64x2_t_m1:
**	ldr	q20, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (q20, int64x2_t, m1)

/*
** load_q20_float16x8_t_1:
**	ldr	q20, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (q20, float16x8_t, 1)

/*
** load_q20_bfloat16x8_t_247:
**	ldr	q20, \[c0, #?247\]
**	ret
*/
LOAD_REG_OFFSET (q20, bfloat16x8_t, 247)

/*
** load_q20_float32x4_t_248:
**	ldr	q20, \[c0, #?248\]
**	ret
*/
LOAD_REG_OFFSET (q20, float32x4_t, 248)

/*
** load_q20_float64x2_t_249:
**	ldr	q20, \[c0, #?249\]
**	ret
*/
LOAD_REG_OFFSET (q20, float64x2_t, 249)

/*
** load_q20_int8x16_t_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	q20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (q20, int8x16_t, 256)

/* Check for valid asm, but don't mandate a parint8x16_tcular sequence.  */
LOAD_REG_INDEX (q20, int8x16_t, int32_t, 1)
LOAD_REG_INDEX (q20, int8x16_t, uint32_t, 1)
LOAD_REG_INDEX (q20, int8x16_t, uint64_t, 1)

LOAD_REG_INDEX (q20, int8x16_t, int32_t, 2)
LOAD_REG_INDEX (q20, int8x16_t, uint32_t, 2)
LOAD_REG_INDEX (q20, int8x16_t, uint64_t, 2)

LOAD_REG_INDEX (q20, int8x16_t, int32_t, 4)
LOAD_REG_INDEX (q20, int8x16_t, uint32_t, 4)
LOAD_REG_INDEX (q20, int8x16_t, uint64_t, 4)

LOAD_REG_INDEX (q20, int8x16_t, int32_t, 8)
LOAD_REG_INDEX (q20, int8x16_t, uint32_t, 8)
LOAD_REG_INDEX (q20, int8x16_t, uint64_t, 8)

LOAD_REG_INDEX (q20, int8x16_t, int32_t, 16)
LOAD_REG_INDEX (q20, int8x16_t, uint32_t, 16)
LOAD_REG_INDEX (q20, int8x16_t, uint64_t, 16)
