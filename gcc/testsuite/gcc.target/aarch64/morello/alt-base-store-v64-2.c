/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <arm_neon.h>

#define ALT_BASE
#include "load-store-utils.h"

/*
** store_zero_int8x8_t_m264:
**	sub	(c[0-9]+), c0, #264
**	str	xzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (int8x8_t, m264)

/*
** store_zero_int16x4_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	xzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (int16x4_t, m257)

/*
** store_zero_int32x2_t_m256:
**	str	xzr, \[c0, #?-256\]
**	ret
*/
STORE_ZERO_OFFSET (int32x2_t, m256)

/*
** store_zero_int64x1_t_m248:
**	str	xzr, \[c0, #?-248\]
**	ret
*/
STORE_ZERO_OFFSET (int64x1_t, m248)

/*
** store_zero_float16x4_t_m8:
**	str	xzr, \[c0, #?-8\]
**	ret
*/
STORE_ZERO_OFFSET (float16x4_t, m8)

/*
** store_zero_float32x2_t_m1:
**	str	xzr, \[c0, #?-1\]
**	ret
*/
STORE_ZERO_OFFSET (float32x2_t, m1)

/*
** store_zero_float32x2_t_1:
**	str	xzr, \[c0, #?1\]
**	ret
*/
STORE_ZERO_OFFSET (float32x2_t, 1)

/*
** store_zero_float64x1_t_8:
**	str	xzr, \[c0, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (float64x1_t, 8)

/*
** store_zero_int8x8_t_248:
**	str	xzr, \[c0, #?248\]
**	ret
*/
STORE_ZERO_OFFSET (int8x8_t, 248)

/*
** store_zero_int8x8_t_255:
**	str	xzr, \[c0, #?255\]
**	ret
*/
STORE_ZERO_OFFSET (int8x8_t, 255)

/*
** store_zero_int8x8_t_256:
**	add	(c[0-9]+), c0, #?256
**	str	xzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (int8x8_t, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_INDEX (int8x8_t, int32_t, 1)
STORE_ZERO_INDEX (int8x8_t, uint32_t, 1)
STORE_ZERO_INDEX (int8x8_t, int8x8_t, 1)

STORE_ZERO_INDEX (int8x8_t, int32_t, 2)
STORE_ZERO_INDEX (int8x8_t, uint32_t, 2)
STORE_ZERO_INDEX (int8x8_t, int8x8_t, 2)

STORE_ZERO_INDEX (int8x8_t, int32_t, 4)
STORE_ZERO_INDEX (int8x8_t, uint32_t, 4)
STORE_ZERO_INDEX (int8x8_t, int8x8_t, 4)

STORE_ZERO_INDEX (int8x8_t, int32_t, 8)
STORE_ZERO_INDEX (int8x8_t, uint32_t, 8)
STORE_ZERO_INDEX (int8x8_t, int8x8_t, 8)

STORE_ZERO_INDEX (int8x8_t, int32_t, 16)
STORE_ZERO_INDEX (int8x8_t, uint32_t, 16)
STORE_ZERO_INDEX (int8x8_t, int8x8_t, 16)
