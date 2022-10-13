/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** store_x10_double_m264:
**	sub	(c[0-9]+), c0, #264
**	str	x10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (x10, double, m264)

/*
** store_x10_double_m257:
**	sub	(c[0-9]+), c0, #257
**	str	x10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (x10, double, m257)

/*
** store_x10_double_m256:
**	str	x10, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (x10, double, m256)

/*
** store_x10_double_m248:
**	str	x10, \[c0, #?-248\]
**	ret
*/
STORE_REG_OFFSET (x10, double, m248)

/*
** store_x10_double_m8:
**	str	x10, \[c0, #?-8\]
**	ret
*/
STORE_REG_OFFSET (x10, double, m8)

/*
** store_x10_double_m1:
**	str	x10, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (x10, double, m1)

/*
** store_x10_double_1:
**	str	x10, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (x10, double, 1)

/*
** store_x10_double_8:
**	str	x10, \[c0, #?8\]
**	ret
*/
STORE_REG_OFFSET (x10, double, 8)

/*
** store_x10_double_248:
**	str	x10, \[c0, #?248\]
**	ret
*/
STORE_REG_OFFSET (x10, double, 248)

/*
** store_x10_double_255:
**	str	x10, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (x10, double, 255)

/*
** store_x10_double_256:
**	add	(c[0-9]+), c0, #?256
**	str	x10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (x10, double, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (x10, double, int32_t, 1)
STORE_REG_INDEX (x10, double, uint32_t, 1)
STORE_REG_INDEX (x10, double, uint64_t, 1)

STORE_REG_INDEX (x10, double, int32_t, 2)
STORE_REG_INDEX (x10, double, uint32_t, 2)
STORE_REG_INDEX (x10, double, uint64_t, 2)

STORE_REG_INDEX (x10, double, int32_t, 4)
STORE_REG_INDEX (x10, double, uint32_t, 4)
STORE_REG_INDEX (x10, double, uint64_t, 4)

STORE_REG_INDEX (x10, double, int32_t, 8)
STORE_REG_INDEX (x10, double, uint32_t, 8)
STORE_REG_INDEX (x10, double, uint64_t, 8)

STORE_REG_INDEX (x10, double, int32_t, 16)
STORE_REG_INDEX (x10, double, uint32_t, 16)
STORE_REG_INDEX (x10, double, uint64_t, 16)

/*
** store_d20_double_m264:
**	sub	(c[0-9]+), c0, #264
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (d20, double, m264)

/*
** store_d20_double_m257:
**	sub	(c[0-9]+), c0, #257
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (d20, double, m257)

/*
** store_d20_double_m256:
**	str	d20, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (d20, double, m256)

/*
** store_d20_double_m248:
**	str	d20, \[c0, #?-248\]
**	ret
*/
STORE_REG_OFFSET (d20, double, m248)

/*
** store_d20_double_m8:
**	str	d20, \[c0, #?-8\]
**	ret
*/
STORE_REG_OFFSET (d20, double, m8)

/*
** store_d20_double_m1:
**	str	d20, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (d20, double, m1)

/*
** store_d20_double_1:
**	str	d20, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (d20, double, 1)

/*
** store_d20_double_8:
**	str	d20, \[c0, #?8\]
**	ret
*/
STORE_REG_OFFSET (d20, double, 8)

/*
** store_d20_double_248:
**	str	d20, \[c0, #?248\]
**	ret
*/
STORE_REG_OFFSET (d20, double, 248)

/*
** store_d20_double_255:
**	str	d20, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (d20, double, 255)

/*
** store_d20_double_256:
**	add	(c[0-9]+), c0, #?256
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (d20, double, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (d10, double, int32_t, 1)
STORE_REG_INDEX (d10, double, uint32_t, 1)
STORE_REG_INDEX (d10, double, uint64_t, 1)

STORE_REG_INDEX (d10, double, int32_t, 2)
STORE_REG_INDEX (d10, double, uint32_t, 2)
STORE_REG_INDEX (d10, double, uint64_t, 2)

STORE_REG_INDEX (d10, double, int32_t, 4)
STORE_REG_INDEX (d10, double, uint32_t, 4)
STORE_REG_INDEX (d10, double, uint64_t, 4)

STORE_REG_INDEX (d10, double, int32_t, 8)
STORE_REG_INDEX (d10, double, uint32_t, 8)
STORE_REG_INDEX (d10, double, uint64_t, 8)

STORE_REG_INDEX (d10, double, int32_t, 16)
STORE_REG_INDEX (d10, double, uint32_t, 16)
STORE_REG_INDEX (d10, double, uint64_t, 16)
