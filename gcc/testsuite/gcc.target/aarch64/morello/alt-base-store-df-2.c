/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** store_zero_double_m264:
**	sub	(c[0-9]+), c0, #264
**	str	xzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (double, m264)

/*
** store_zero_double_m257:
**	sub	(c[0-9]+), c0, #257
**	str	xzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (double, m257)

/*
** store_zero_double_m256:
**	str	xzr, \[c0, #?-256\]
**	ret
*/
STORE_ZERO_OFFSET (double, m256)

/*
** store_zero_double_m248:
**	str	xzr, \[c0, #?-248\]
**	ret
*/
STORE_ZERO_OFFSET (double, m248)

/*
** store_zero_double_m8:
**	str	xzr, \[c0, #?-8\]
**	ret
*/
STORE_ZERO_OFFSET (double, m8)

/*
** store_zero_double_m1:
**	str	xzr, \[c0, #?-1\]
**	ret
*/
STORE_ZERO_OFFSET (double, m1)

/*
** store_zero_double_1:
**	str	xzr, \[c0, #?1\]
**	ret
*/
STORE_ZERO_OFFSET (double, 1)

/*
** store_zero_double_8:
**	str	xzr, \[c0, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (double, 8)

/*
** store_zero_double_248:
**	str	xzr, \[c0, #?248\]
**	ret
*/
STORE_ZERO_OFFSET (double, 248)

/*
** store_zero_double_255:
**	str	xzr, \[c0, #?255\]
**	ret
*/
STORE_ZERO_OFFSET (double, 255)

/*
** store_zero_double_256:
**	add	(c[0-9]+), c0, #?256
**	str	xzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (double, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_INDEX (double, int32_t, 1)
STORE_ZERO_INDEX (double, uint32_t, 1)
STORE_ZERO_INDEX (double, uint64_t, 1)

STORE_ZERO_INDEX (double, int32_t, 2)
STORE_ZERO_INDEX (double, uint32_t, 2)
STORE_ZERO_INDEX (double, uint64_t, 2)

STORE_ZERO_INDEX (double, int32_t, 4)
STORE_ZERO_INDEX (double, uint32_t, 4)
STORE_ZERO_INDEX (double, uint64_t, 4)

STORE_ZERO_INDEX (double, int32_t, 8)
STORE_ZERO_INDEX (double, uint32_t, 8)
STORE_ZERO_INDEX (double, uint64_t, 8)

STORE_ZERO_INDEX (double, int32_t, 16)
STORE_ZERO_INDEX (double, uint32_t, 16)
STORE_ZERO_INDEX (double, uint64_t, 16)
