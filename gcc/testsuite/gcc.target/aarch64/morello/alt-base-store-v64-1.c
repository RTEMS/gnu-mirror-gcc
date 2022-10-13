/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <arm_neon.h>

#define ALT_BASE
#include "load-store-utils.h"

/*
** store_x10_int8x8_t_m264:
**	sub	(c[0-9]+), c0, #264
**	str	x10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (x10, int8x8_t, m264)

/*
** store_x10_int16x4_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	x10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (x10, int16x4_t, m257)

/*
** store_x10_int32x2_t_m256:
**	str	x10, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (x10, int32x2_t, m256)

/*
** store_x10_int64x1_t_m248:
**	str	x10, \[c0, #?-248\]
**	ret
*/
STORE_REG_OFFSET (x10, int64x1_t, m248)

/*
** store_x10_float16x4_t_m8:
**	str	x10, \[c0, #?-8\]
**	ret
*/
STORE_REG_OFFSET (x10, float16x4_t, m8)

/*
** store_x10_bfloat16x4_t_m1:
**	str	x10, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (x10, bfloat16x4_t, m1)

/*
** store_x10_float32x2_t_1:
**	str	x10, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (x10, float32x2_t, 1)

/*
** store_x10_float64x1_t_8:
**	str	x10, \[c0, #?8\]
**	ret
*/
STORE_REG_OFFSET (x10, float64x1_t, 8)

/*
** store_x10_int8x8_t_248:
**	str	x10, \[c0, #?248\]
**	ret
*/
STORE_REG_OFFSET (x10, int8x8_t, 248)

/*
** store_x10_int8x8_t_255:
**	str	x10, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (x10, int8x8_t, 255)

/*
** store_x10_int8x8_t_256:
**	add	(c[0-9]+), c0, #?256
**	str	x10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (x10, int8x8_t, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (x10, int8x8_t, int32_t, 1)
STORE_REG_INDEX (x10, int8x8_t, uint32_t, 1)
STORE_REG_INDEX (x10, int8x8_t, int8x8_t, 1)

STORE_REG_INDEX (x10, int8x8_t, int32_t, 2)
STORE_REG_INDEX (x10, int8x8_t, uint32_t, 2)
STORE_REG_INDEX (x10, int8x8_t, int8x8_t, 2)

STORE_REG_INDEX (x10, int8x8_t, int32_t, 4)
STORE_REG_INDEX (x10, int8x8_t, uint32_t, 4)
STORE_REG_INDEX (x10, int8x8_t, int8x8_t, 4)

STORE_REG_INDEX (x10, int8x8_t, int32_t, 8)
STORE_REG_INDEX (x10, int8x8_t, uint32_t, 8)
STORE_REG_INDEX (x10, int8x8_t, int8x8_t, 8)

STORE_REG_INDEX (x10, int8x8_t, int32_t, 16)
STORE_REG_INDEX (x10, int8x8_t, uint32_t, 16)
STORE_REG_INDEX (x10, int8x8_t, int8x8_t, 16)

/*
** store_d20_int8x8_t_m264:
**	sub	(c[0-9]+), c0, #264
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (d20, int8x8_t, m264)

/*
** store_d20_int16x4_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (d20, int16x4_t, m257)

/*
** store_d20_int32x2_t_m256:
**	str	d20, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (d20, int32x2_t, m256)

/*
** store_d20_int64x1_t_m248:
**	str	d20, \[c0, #?-248\]
**	ret
*/
STORE_REG_OFFSET (d20, int64x1_t, m248)

/*
** store_d20_float16x4_t_m8:
**	str	d20, \[c0, #?-8\]
**	ret
*/
STORE_REG_OFFSET (d20, float16x4_t, m8)

/*
** store_d20_bfloat16x4_t_m1:
**	str	d20, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (d20, bfloat16x4_t, m1)

/*
** store_d20_float32x2_t_1:
**	str	d20, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (d20, float32x2_t, 1)

/*
** store_d20_float64x1_t_8:
**	str	d20, \[c0, #?8\]
**	ret
*/
STORE_REG_OFFSET (d20, float64x1_t, 8)

/*
** store_d20_int8x8_t_248:
**	str	d20, \[c0, #?248\]
**	ret
*/
STORE_REG_OFFSET (d20, int8x8_t, 248)

/*
** store_d20_int8x8_t_255:
**	str	d20, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (d20, int8x8_t, 255)

/*
** store_d20_int8x8_t_256:
**	add	(c[0-9]+), c0, #?256
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (d20, int8x8_t, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (d20, int8x8_t, int32_t, 1)
STORE_REG_INDEX (d20, int8x8_t, uint32_t, 1)
STORE_REG_INDEX (d20, int8x8_t, int8x8_t, 1)

STORE_REG_INDEX (d20, int8x8_t, int32_t, 2)
STORE_REG_INDEX (d20, int8x8_t, uint32_t, 2)
STORE_REG_INDEX (d20, int8x8_t, int8x8_t, 2)

STORE_REG_INDEX (d20, int8x8_t, int32_t, 4)
STORE_REG_INDEX (d20, int8x8_t, uint32_t, 4)
STORE_REG_INDEX (d20, int8x8_t, int8x8_t, 4)

STORE_REG_INDEX (d20, int8x8_t, int32_t, 8)
STORE_REG_INDEX (d20, int8x8_t, uint32_t, 8)
STORE_REG_INDEX (d20, int8x8_t, int8x8_t, 8)

STORE_REG_INDEX (d20, int8x8_t, int32_t, 16)
STORE_REG_INDEX (d20, int8x8_t, uint32_t, 16)
STORE_REG_INDEX (d20, int8x8_t, int8x8_t, 16)
