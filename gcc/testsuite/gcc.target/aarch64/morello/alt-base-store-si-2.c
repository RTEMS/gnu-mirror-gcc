/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** store_zero_uint32_t_m260:
**	sub	(c[0-9]+), c0, #260
**	str	wzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, m260)

/*
** store_zero_uint32_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	wzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, m257)

/*
** store_zero_uint32_t_m256:
**	str	wzr, \[c0, #?-256\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, m256)

/*
** store_zero_uint32_t_m252:
**	str	wzr, \[c0, #?-252\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, m252)

/*
** store_zero_uint32_t_m4:
**	str	wzr, \[c0, #?-4\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, m4)

/*
** store_zero_uint32_t_m1:
**	str	wzr, \[c0, #?-1\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, m1)

/*
** store_zero_uint32_t_1:
**	str	wzr, \[c0, #?1\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, 1)

/*
** store_zero_uint32_t_4:
**	str	wzr, \[c0, #?4\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, 4)

/*
** store_zero_uint32_t_252:
**	str	wzr, \[c0, #?252\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, 252)

/*
** store_zero_uint32_t_255:
**	str	wzr, \[c0, #?255\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, 255)

/*
** store_zero_uint32_t_256:
**	str	wzr, \[c0, #?256\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, 256)

/*
** store_zero_uint32_t_257:
**	add	(c[0-9]+), c0, #?256
**	str	wzr, \[\1, #?1\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, 257)

/*
** store_zero_uint32_t_260:
**	str	wzr, \[c0, #?260\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, 260)

/*
** store_zero_uint32_t_2044:
**	str	wzr, \[c0, #?2044\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, 2044)

/*
** store_zero_uint32_t_2048:
**	add	(c[0-9]+), c0, #?2048
**	str	wzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (uint32_t, 2048)

/*
** store_zero_uint32_t_int32_t_1:
**	str	wzr, \[c0, w1, sxtw\]
**	ret
*/
STORE_ZERO_INDEX (uint32_t, int32_t, 1)

/*
** store_zero_uint32_t_uint32_t_1:
**	str	wzr, \[c0, w1, uxtw\]
**	ret
*/
STORE_ZERO_INDEX (uint32_t, uint32_t, 1)

/*
** store_zero_uint32_t_uint64_t_1:
**	str	wzr, \[c0, x1\]
**	ret
*/
STORE_ZERO_INDEX (uint32_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_INDEX (uint32_t, int32_t, 2)
STORE_ZERO_INDEX (uint32_t, uint32_t, 2)
STORE_ZERO_INDEX (uint32_t, uint64_t, 2)

/*
** store_zero_uint32_t_int32_t_4:
**	str	wzr, \[c0, w1, sxtw #?2\]
**	ret
*/
STORE_ZERO_INDEX (uint32_t, int32_t, 4)

/*
** store_zero_uint32_t_uint32_t_4:
**	str	wzr, \[c0, w1, uxtw #?2\]
**	ret
*/
STORE_ZERO_INDEX (uint32_t, uint32_t, 4)

/*
** store_zero_uint32_t_uint64_t_4:
**	str	wzr, \[c0, x1, lsl #?2\]
**	ret
*/
STORE_ZERO_INDEX (uint32_t, uint64_t, 4)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_INDEX (uint32_t, int32_t, 8)
STORE_ZERO_INDEX (uint32_t, uint32_t, 8)
STORE_ZERO_INDEX (uint32_t, uint64_t, 8)

STORE_ZERO_INDEX (uint32_t, int32_t, 16)
STORE_ZERO_INDEX (uint32_t, uint32_t, 16)
STORE_ZERO_INDEX (uint32_t, uint64_t, 16)
