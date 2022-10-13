/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** store_x10_uint64_t_m264:
**	sub	(c[0-9]+), c0, #264
**	str	x10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, m264)

/*
** store_x10_uint64_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	x10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, m257)

/*
** store_x10_uint64_t_m256:
**	str	x10, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, m256)

/*
** store_x10_uint64_t_m248:
**	str	x10, \[c0, #?-248\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, m248)

/*
** store_x10_uint64_t_m8:
**	str	x10, \[c0, #?-8\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, m8)

/*
** store_x10_uint64_t_m1:
**	str	x10, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, m1)

/*
** store_x10_uint64_t_1:
**	str	x10, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, 1)

/*
** store_x10_uint64_t_8:
**	str	x10, \[c0, #?8\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, 8)

/*
** store_x10_uint64_t_248:
**	str	x10, \[c0, #?248\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, 248)

/*
** store_x10_uint64_t_255:
**	str	x10, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, 255)

/*
** store_x10_uint64_t_256:
**	str	x10, \[c0, #?256\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, 256)

/*
** store_x10_uint64_t_257:
**	add	(c[0-9]+), c0, #?256
**	str	x10, \[\1, #?1\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, 257)

/*
** store_x10_uint64_t_264:
**	str	x10, \[c0, #?264\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, 264)

/*
** store_x10_uint64_t_4088:
**	str	x10, \[c0, #?4088\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, 4088)

/*
** store_x10_uint64_t_4096:
**	add	(c[0-9]+), c0, #?4096
**	str	x10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (x10, uint64_t, 4096)

/*
** store_x10_uint64_t_int32_t_1:
**	str	x10, \[c0, w1, sxtw\]
**	ret
*/
STORE_REG_INDEX (x10, uint64_t, int32_t, 1)

/*
** store_x10_uint64_t_uint32_t_1:
**	str	x10, \[c0, w1, uxtw\]
**	ret
*/
STORE_REG_INDEX (x10, uint64_t, uint32_t, 1)

/*
** store_x10_uint64_t_uint64_t_1:
**	str	x10, \[c0, x1\]
**	ret
*/
STORE_REG_INDEX (x10, uint64_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (x10, uint64_t, int32_t, 2)
STORE_REG_INDEX (x10, uint64_t, uint32_t, 2)
STORE_REG_INDEX (x10, uint64_t, uint64_t, 2)

STORE_REG_INDEX (x10, uint64_t, int32_t, 4)
STORE_REG_INDEX (x10, uint64_t, uint32_t, 4)
STORE_REG_INDEX (x10, uint64_t, uint64_t, 4)

/*
** store_x10_uint64_t_int32_t_8:
**	str	x10, \[c0, w1, sxtw #?3\]
**	ret
*/
STORE_REG_INDEX (x10, uint64_t, int32_t, 8)

/*
** store_x10_uint64_t_uint32_t_8:
**	str	x10, \[c0, w1, uxtw #?3\]
**	ret
*/
STORE_REG_INDEX (x10, uint64_t, uint32_t, 8)

/*
** store_x10_uint64_t_uint64_t_8:
**	str	x10, \[c0, x1, lsl #?3\]
**	ret
*/
STORE_REG_INDEX (x10, uint64_t, uint64_t, 8)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (x10, uint64_t, int32_t, 16)
STORE_REG_INDEX (x10, uint64_t, uint32_t, 16)
STORE_REG_INDEX (x10, uint64_t, uint64_t, 16)

/*
** store_d20_uint64_t_m264:
**	sub	(c[0-9]+), c0, #264
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (d20, uint64_t, m264)

/*
** store_d20_uint64_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (d20, uint64_t, m257)

/*
** store_d20_uint64_t_m256:
**	str	d20, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (d20, uint64_t, m256)

/*
** store_d20_uint64_t_m248:
**	str	d20, \[c0, #?-248\]
**	ret
*/
STORE_REG_OFFSET (d20, uint64_t, m248)

/*
** store_d20_uint64_t_m8:
**	str	d20, \[c0, #?-8\]
**	ret
*/
STORE_REG_OFFSET (d20, uint64_t, m8)

/*
** store_d20_uint64_t_m1:
**	str	d20, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (d20, uint64_t, m1)

/*
** store_d20_uint64_t_1:
**	str	d20, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (d20, uint64_t, 1)

/*
** store_d20_uint64_t_8:
**	str	d20, \[c0, #?8\]
**	ret
*/
STORE_REG_OFFSET (d20, uint64_t, 8)

/*
** store_d20_uint64_t_248:
**	str	d20, \[c0, #?248\]
**	ret
*/
STORE_REG_OFFSET (d20, uint64_t, 248)

/*
** store_d20_uint64_t_255:
**	str	d20, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (d20, uint64_t, 255)

/*
** store_d20_uint64_t_256:
**	add	(c[0-9]+), c0, #?256
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (d20, uint64_t, 256)

/*
** store_d20_uint64_t_int32_t_1:
**	add	(c[0-9]+), c0, w1, sxtw
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_INDEX (d20, uint64_t, int32_t, 1)

/*
** store_d20_uint64_t_uint32_t_1:
**	add	(c[0-9]+), c0, w1, uxtw
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_INDEX (d20, uint64_t, uint32_t, 1)

/*
** store_d20_uint64_t_uint64_t_1:
**	add	(c[0-9]+), c0, x1
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_INDEX (d20, uint64_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (d20, uint64_t, int32_t, 2)
STORE_REG_INDEX (d20, uint64_t, uint32_t, 2)
STORE_REG_INDEX (d20, uint64_t, uint64_t, 2)

STORE_REG_INDEX (d20, uint64_t, int32_t, 4)
STORE_REG_INDEX (d20, uint64_t, uint32_t, 4)
STORE_REG_INDEX (d20, uint64_t, uint64_t, 4)

/*
** store_d20_uint64_t_int32_t_8:
**	add	(c[0-9]+), c0, w1, sxtw #?3
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_INDEX (d20, uint64_t, int32_t, 8)

/*
** store_d20_uint64_t_uint32_t_8:
**	add	(c[0-9]+), c0, w1, uxtw #?3
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_INDEX (d20, uint64_t, uint32_t, 8)

/*
** store_d20_uint64_t_uint64_t_8:
**	add	(c[0-9]+), c0, x1, lsl #?3
**	str	d20, \[\1\]
**	ret
*/
STORE_REG_INDEX (d20, uint64_t, uint64_t, 8)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (d20, uint64_t, int32_t, 16)
STORE_REG_INDEX (d20, uint64_t, uint32_t, 16)
STORE_REG_INDEX (d20, uint64_t, uint64_t, 16)
