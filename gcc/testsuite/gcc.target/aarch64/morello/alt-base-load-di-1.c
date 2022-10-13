/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** load_x10_uint64_t_m264:
**	sub	(c[0-9]+), c0, #264
**	ldr	x10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, m264)

/*
** load_x10_uint64_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	x10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, m257)

/*
** load_x10_uint64_t_m256:
**	ldr	x10, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, m256)

/*
** load_x10_uint64_t_m248:
**	ldr	x10, \[c0, #?-248\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, m248)

/*
** load_x10_uint64_t_m8:
**	ldr	x10, \[c0, #?-8\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, m8)

/*
** load_x10_uint64_t_m1:
**	ldr	x10, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, m1)

/*
** load_x10_uint64_t_1:
**	ldr	x10, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, 1)

/*
** load_x10_uint64_t_8:
**	ldr	x10, \[c0, #?8\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, 8)

/*
** load_x10_uint64_t_248:
**	ldr	x10, \[c0, #?248\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, 248)

/*
** load_x10_uint64_t_255:
**	ldr	x10, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, 255)

/*
** load_x10_uint64_t_256:
**	ldr	x10, \[c0, #?256\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, 256)

/*
** load_x10_uint64_t_257:
**	add	(c[0-9]+), c0, #?256
**	ldr	x10, \[\1, #?1\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, 257)

/*
** load_x10_uint64_t_264:
**	ldr	x10, \[c0, #?264\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, 264)

/*
** load_x10_uint64_t_4088:
**	ldr	x10, \[c0, #?4088\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, 4088)

/*
** load_x10_uint64_t_4096:
**	add	(c[0-9]+), c0, #?4096
**	ldr	x10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (x10, uint64_t, 4096)

/*
** load_x10_uint64_t_int32_t_1:
**	ldr	x10, \[c0, w1, sxtw\]
**	ret
*/
LOAD_REG_INDEX (x10, uint64_t, int32_t, 1)

/*
** load_x10_uint64_t_uint32_t_1:
**	ldr	x10, \[c0, w1, uxtw\]
**	ret
*/
LOAD_REG_INDEX (x10, uint64_t, uint32_t, 1)

/*
** load_x10_uint64_t_uint64_t_1:
**	ldr	x10, \[c0, x1\]
**	ret
*/
LOAD_REG_INDEX (x10, uint64_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (x10, uint64_t, int32_t, 2)
LOAD_REG_INDEX (x10, uint64_t, uint32_t, 2)
LOAD_REG_INDEX (x10, uint64_t, uint64_t, 2)

LOAD_REG_INDEX (x10, uint64_t, int32_t, 4)
LOAD_REG_INDEX (x10, uint64_t, uint32_t, 4)
LOAD_REG_INDEX (x10, uint64_t, uint64_t, 4)

/*
** load_x10_uint64_t_int32_t_8:
**	ldr	x10, \[c0, w1, sxtw #?3\]
**	ret
*/
LOAD_REG_INDEX (x10, uint64_t, int32_t, 8)

/*
** load_x10_uint64_t_uint32_t_8:
**	ldr	x10, \[c0, w1, uxtw #?3\]
**	ret
*/
LOAD_REG_INDEX (x10, uint64_t, uint32_t, 8)

/*
** load_x10_uint64_t_uint64_t_8:
**	ldr	x10, \[c0, x1, lsl #?3\]
**	ret
*/
LOAD_REG_INDEX (x10, uint64_t, uint64_t, 8)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (x10, uint64_t, int32_t, 16)
LOAD_REG_INDEX (x10, uint64_t, uint32_t, 16)
LOAD_REG_INDEX (x10, uint64_t, uint64_t, 16)

/*
** load_d20_uint64_t_m264:
**	sub	(c[0-9]+), c0, #264
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (d20, uint64_t, m264)

/*
** load_d20_uint64_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (d20, uint64_t, m257)

/*
** load_d20_uint64_t_m256:
**	ldr	d20, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (d20, uint64_t, m256)

/*
** load_d20_uint64_t_m248:
**	ldr	d20, \[c0, #?-248\]
**	ret
*/
LOAD_REG_OFFSET (d20, uint64_t, m248)

/*
** load_d20_uint64_t_m8:
**	ldr	d20, \[c0, #?-8\]
**	ret
*/
LOAD_REG_OFFSET (d20, uint64_t, m8)

/*
** load_d20_uint64_t_m1:
**	ldr	d20, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (d20, uint64_t, m1)

/*
** load_d20_uint64_t_1:
**	ldr	d20, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (d20, uint64_t, 1)

/*
** load_d20_uint64_t_8:
**	ldr	d20, \[c0, #?8\]
**	ret
*/
LOAD_REG_OFFSET (d20, uint64_t, 8)

/*
** load_d20_uint64_t_248:
**	ldr	d20, \[c0, #?248\]
**	ret
*/
LOAD_REG_OFFSET (d20, uint64_t, 248)

/*
** load_d20_uint64_t_255:
**	ldr	d20, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (d20, uint64_t, 255)

/*
** load_d20_uint64_t_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (d20, uint64_t, 256)

/*
** load_d20_uint64_t_int32_t_1:
**	add	(c[0-9]+), c0, w1, sxtw
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (d20, uint64_t, int32_t, 1)

/*
** load_d20_uint64_t_uint32_t_1:
**	add	(c[0-9]+), c0, w1, uxtw
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (d20, uint64_t, uint32_t, 1)

/*
** load_d20_uint64_t_uint64_t_1:
**	add	(c[0-9]+), c0, x1
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (d20, uint64_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (d20, uint64_t, int32_t, 2)
LOAD_REG_INDEX (d20, uint64_t, uint32_t, 2)
LOAD_REG_INDEX (d20, uint64_t, uint64_t, 2)

LOAD_REG_INDEX (d20, uint64_t, int32_t, 4)
LOAD_REG_INDEX (d20, uint64_t, uint32_t, 4)
LOAD_REG_INDEX (d20, uint64_t, uint64_t, 4)

/*
** load_d20_uint64_t_int32_t_8:
**	add	(c[0-9]+), c0, w1, sxtw #?3
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (d20, uint64_t, int32_t, 8)

/*
** load_d20_uint64_t_uint32_t_8:
**	add	(c[0-9]+), c0, w1, uxtw #?3
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (d20, uint64_t, uint32_t, 8)

/*
** load_d20_uint64_t_uint64_t_8:
**	add	(c[0-9]+), c0, x1, lsl #?3
**	ldr	d20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (d20, uint64_t, uint64_t, 8)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (d20, uint64_t, int32_t, 16)
LOAD_REG_INDEX (d20, uint64_t, uint32_t, 16)
LOAD_REG_INDEX (d20, uint64_t, uint64_t, 16)
