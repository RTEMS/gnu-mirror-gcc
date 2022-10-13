/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** store_zero_uint64_t_m264:
**	sub	(c[0-9]+), c0, #264
**	str	xzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, m264)

/*
** store_zero_uint64_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	xzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, m257)

/*
** store_zero_uint64_t_m256:
**	str	xzr, \[c0, #?-256\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, m256)

/*
** store_zero_uint64_t_m248:
**	str	xzr, \[c0, #?-248\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, m248)

/*
** store_zero_uint64_t_m8:
**	str	xzr, \[c0, #?-8\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, m8)

/*
** store_zero_uint64_t_m1:
**	str	xzr, \[c0, #?-1\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, m1)

/*
** store_zero_uint64_t_1:
**	str	xzr, \[c0, #?1\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, 1)

/*
** store_zero_uint64_t_8:
**	str	xzr, \[c0, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, 8)

/*
** store_zero_uint64_t_248:
**	str	xzr, \[c0, #?248\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, 248)

/*
** store_zero_uint64_t_255:
**	str	xzr, \[c0, #?255\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, 255)

/*
** store_zero_uint64_t_256:
**	str	xzr, \[c0, #?256\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, 256)

/*
** store_zero_uint64_t_257:
**	add	(c[0-9]+), c0, #?256
**	str	xzr, \[\1, #?1\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, 257)

/*
** store_zero_uint64_t_264:
**	str	xzr, \[c0, #?264\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, 264)

/*
** store_zero_uint64_t_4088:
**	str	xzr, \[c0, #?4088\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, 4088)

/*
** store_zero_uint64_t_4096:
**	add	(c[0-9]+), c0, #?4096
**	str	xzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (uint64_t, 4096)

/*
** store_zero_uint64_t_int32_t_1:
**	str	xzr, \[c0, w1, sxtw\]
**	ret
*/
STORE_ZERO_INDEX (uint64_t, int32_t, 1)

/*
** store_zero_uint64_t_uint32_t_1:
**	str	xzr, \[c0, w1, uxtw\]
**	ret
*/
STORE_ZERO_INDEX (uint64_t, uint32_t, 1)

/*
** store_zero_uint64_t_uint64_t_1:
**	str	xzr, \[c0, x1\]
**	ret
*/
STORE_ZERO_INDEX (uint64_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_INDEX (uint64_t, int32_t, 2)
STORE_ZERO_INDEX (uint64_t, uint32_t, 2)
STORE_ZERO_INDEX (uint64_t, uint64_t, 2)

STORE_ZERO_INDEX (uint64_t, int32_t, 4)
STORE_ZERO_INDEX (uint64_t, uint32_t, 4)
STORE_ZERO_INDEX (uint64_t, uint64_t, 4)

/*
** store_zero_uint64_t_int32_t_8:
**	str	xzr, \[c0, w1, sxtw #?3\]
**	ret
*/
STORE_ZERO_INDEX (uint64_t, int32_t, 8)

/*
** store_zero_uint64_t_uint32_t_8:
**	str	xzr, \[c0, w1, uxtw #?3\]
**	ret
*/
STORE_ZERO_INDEX (uint64_t, uint32_t, 8)

/*
** store_zero_uint64_t_uint64_t_8:
**	str	xzr, \[c0, x1, lsl #?3\]
**	ret
*/
STORE_ZERO_INDEX (uint64_t, uint64_t, 8)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_INDEX (uint64_t, int32_t, 16)
STORE_ZERO_INDEX (uint64_t, uint32_t, 16)
STORE_ZERO_INDEX (uint64_t, uint64_t, 16)
