/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <arm_neon.h>

#define ALT_BASE
#include "load-store-utils.h"

/*
** load_x10_int8x8_t_m264:
**	sub	(c[0-9]+), c0, #264
**	ldr	x10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (x10, int8x8_t, m264)

/*
** load_x10_int16x4_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	x10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (x10, int16x4_t, m257)

/*
** load_x10_int32x2_t_m256:
**	ldr	x10, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (x10, int32x2_t, m256)

/*
** load_x10_int64x1_t_m248:
**	ldr	x10, \[c0, #?-248\]
**	ret
*/
LOAD_REG_OFFSET (x10, int64x1_t, m248)

/*
** load_x10_float16x4_t_m8:
**	ldr	x10, \[c0, #?-8\]
**	ret
*/
LOAD_REG_OFFSET (x10, float16x4_t, m8)

/*
** load_x10_bfloat16x4_t_m1:
**	ldr	x10, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (x10, bfloat16x4_t, m1)

/*
** load_x10_float32x2_t_1:
**	ldr	x10, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (x10, float32x2_t, 1)

/*
** load_x10_float64x1_t_8:
**	ldr	x10, \[c0, #?8\]
**	ret
*/
LOAD_REG_OFFSET (x10, float64x1_t, 8)

/*
** load_x10_int8x8_t_248:
**	ldr	x10, \[c0, #?248\]
**	ret
*/
LOAD_REG_OFFSET (x10, int8x8_t, 248)

/*
** load_x10_int8x8_t_255:
**	ldr	x10, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (x10, int8x8_t, 255)

/*
** load_x10_int8x8_t_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	x10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (x10, int8x8_t, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (x10, int8x8_t, int32_t, 1)
LOAD_REG_INDEX (x10, int8x8_t, uint32_t, 1)
LOAD_REG_INDEX (x10, int8x8_t, int8x8_t, 1)

LOAD_REG_INDEX (x10, int8x8_t, int32_t, 2)
LOAD_REG_INDEX (x10, int8x8_t, uint32_t, 2)
LOAD_REG_INDEX (x10, int8x8_t, int8x8_t, 2)

LOAD_REG_INDEX (x10, int8x8_t, int32_t, 4)
LOAD_REG_INDEX (x10, int8x8_t, uint32_t, 4)
LOAD_REG_INDEX (x10, int8x8_t, int8x8_t, 4)

LOAD_REG_INDEX (x10, int8x8_t, int32_t, 8)
LOAD_REG_INDEX (x10, int8x8_t, uint32_t, 8)
LOAD_REG_INDEX (x10, int8x8_t, int8x8_t, 8)

LOAD_REG_INDEX (x10, int8x8_t, int32_t, 16)
LOAD_REG_INDEX (x10, int8x8_t, uint32_t, 16)
LOAD_REG_INDEX (x10, int8x8_t, int8x8_t, 16)

/*
** load_d20_int8x8_t_m264:
**	sub	(c[0-9]+), c0, #264
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (d20, int8x8_t, m264)

/*
** load_d20_int16x4_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (d20, int16x4_t, m257)

/*
** load_d20_int32x2_t_m256:
**	ldr	d20, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (d20, int32x2_t, m256)

/*
** load_d20_int64x1_t_m248:
**	ldr	d20, \[c0, #?-248\]
**	ret
*/
LOAD_REG_OFFSET (d20, int64x1_t, m248)

/*
** load_d20_float16x4_t_m8:
**	ldr	d20, \[c0, #?-8\]
**	ret
*/
LOAD_REG_OFFSET (d20, float16x4_t, m8)

/*
** load_d20_bfloat16x4_t_m1:
**	ldr	d20, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (d20, bfloat16x4_t, m1)

/*
** load_d20_float32x2_t_1:
**	ldr	d20, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (d20, float32x2_t, 1)

/*
** load_d20_float64x1_t_8:
**	ldr	d20, \[c0, #?8\]
**	ret
*/
LOAD_REG_OFFSET (d20, float64x1_t, 8)

/*
** load_d20_int8x8_t_248:
**	ldr	d20, \[c0, #?248\]
**	ret
*/
LOAD_REG_OFFSET (d20, int8x8_t, 248)

/*
** load_d20_int8x8_t_255:
**	ldr	d20, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (d20, int8x8_t, 255)

/*
** load_d20_int8x8_t_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (d20, int8x8_t, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (d20, int8x8_t, int32_t, 1)
LOAD_REG_INDEX (d20, int8x8_t, uint32_t, 1)
LOAD_REG_INDEX (d20, int8x8_t, int8x8_t, 1)

LOAD_REG_INDEX (d20, int8x8_t, int32_t, 2)
LOAD_REG_INDEX (d20, int8x8_t, uint32_t, 2)
LOAD_REG_INDEX (d20, int8x8_t, int8x8_t, 2)

LOAD_REG_INDEX (d20, int8x8_t, int32_t, 4)
LOAD_REG_INDEX (d20, int8x8_t, uint32_t, 4)
LOAD_REG_INDEX (d20, int8x8_t, int8x8_t, 4)

LOAD_REG_INDEX (d20, int8x8_t, int32_t, 8)
LOAD_REG_INDEX (d20, int8x8_t, uint32_t, 8)
LOAD_REG_INDEX (d20, int8x8_t, int8x8_t, 8)

LOAD_REG_INDEX (d20, int8x8_t, int32_t, 16)
LOAD_REG_INDEX (d20, int8x8_t, uint32_t, 16)
LOAD_REG_INDEX (d20, int8x8_t, int8x8_t, 16)
