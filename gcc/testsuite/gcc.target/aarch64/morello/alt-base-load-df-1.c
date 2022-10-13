/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** load_x10_double_m264:
**	sub	(c[0-9]+), c0, #264
**	ldr	x10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (x10, double, m264)

/*
** load_x10_double_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	x10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (x10, double, m257)

/*
** load_x10_double_m256:
**	ldr	x10, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (x10, double, m256)

/*
** load_x10_double_m248:
**	ldr	x10, \[c0, #?-248\]
**	ret
*/
LOAD_REG_OFFSET (x10, double, m248)

/*
** load_x10_double_m8:
**	ldr	x10, \[c0, #?-8\]
**	ret
*/
LOAD_REG_OFFSET (x10, double, m8)

/*
** load_x10_double_m1:
**	ldr	x10, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (x10, double, m1)

/*
** load_x10_double_1:
**	ldr	x10, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (x10, double, 1)

/*
** load_x10_double_8:
**	ldr	x10, \[c0, #?8\]
**	ret
*/
LOAD_REG_OFFSET (x10, double, 8)

/*
** load_x10_double_248:
**	ldr	x10, \[c0, #?248\]
**	ret
*/
LOAD_REG_OFFSET (x10, double, 248)

/*
** load_x10_double_255:
**	ldr	x10, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (x10, double, 255)

/*
** load_x10_double_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	x10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (x10, double, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (x10, double, int32_t, 1)
LOAD_REG_INDEX (x10, double, uint32_t, 1)
LOAD_REG_INDEX (x10, double, uint64_t, 1)

LOAD_REG_INDEX (x10, double, int32_t, 2)
LOAD_REG_INDEX (x10, double, uint32_t, 2)
LOAD_REG_INDEX (x10, double, uint64_t, 2)

LOAD_REG_INDEX (x10, double, int32_t, 4)
LOAD_REG_INDEX (x10, double, uint32_t, 4)
LOAD_REG_INDEX (x10, double, uint64_t, 4)

LOAD_REG_INDEX (x10, double, int32_t, 8)
LOAD_REG_INDEX (x10, double, uint32_t, 8)
LOAD_REG_INDEX (x10, double, uint64_t, 8)

LOAD_REG_INDEX (x10, double, int32_t, 16)
LOAD_REG_INDEX (x10, double, uint32_t, 16)
LOAD_REG_INDEX (x10, double, uint64_t, 16)

/*
** load_d20_double_m264:
**	sub	(c[0-9]+), c0, #264
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (d20, double, m264)

/*
** load_d20_double_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (d20, double, m257)

/*
** load_d20_double_m256:
**	ldr	d20, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (d20, double, m256)

/*
** load_d20_double_m248:
**	ldr	d20, \[c0, #?-248\]
**	ret
*/
LOAD_REG_OFFSET (d20, double, m248)

/*
** load_d20_double_m8:
**	ldr	d20, \[c0, #?-8\]
**	ret
*/
LOAD_REG_OFFSET (d20, double, m8)

/*
** load_d20_double_m1:
**	ldr	d20, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (d20, double, m1)

/*
** load_d20_double_1:
**	ldr	d20, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (d20, double, 1)

/*
** load_d20_double_8:
**	ldr	d20, \[c0, #?8\]
**	ret
*/
LOAD_REG_OFFSET (d20, double, 8)

/*
** load_d20_double_248:
**	ldr	d20, \[c0, #?248\]
**	ret
*/
LOAD_REG_OFFSET (d20, double, 248)

/*
** load_d20_double_255:
**	ldr	d20, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (d20, double, 255)

/*
** load_d20_double_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (d20, double, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (d10, double, int32_t, 1)
LOAD_REG_INDEX (d10, double, uint32_t, 1)
LOAD_REG_INDEX (d10, double, uint64_t, 1)

LOAD_REG_INDEX (d10, double, int32_t, 2)
LOAD_REG_INDEX (d10, double, uint32_t, 2)
LOAD_REG_INDEX (d10, double, uint64_t, 2)

LOAD_REG_INDEX (d10, double, int32_t, 4)
LOAD_REG_INDEX (d10, double, uint32_t, 4)
LOAD_REG_INDEX (d10, double, uint64_t, 4)

LOAD_REG_INDEX (d10, double, int32_t, 8)
LOAD_REG_INDEX (d10, double, uint32_t, 8)
LOAD_REG_INDEX (d10, double, uint64_t, 8)

LOAD_REG_INDEX (d10, double, int32_t, 16)
LOAD_REG_INDEX (d10, double, uint32_t, 16)
LOAD_REG_INDEX (d10, double, uint64_t, 16)
