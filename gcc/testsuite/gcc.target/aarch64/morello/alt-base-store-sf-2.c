/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** store_zero_float_m260:
**	sub	(c[0-9]+), c0, #260
**	str	wzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (float, m260)

/*
** store_zero_float_m257:
**	sub	(c[0-9]+), c0, #257
**	str	wzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (float, m257)

/*
** store_zero_float_m256:
**	str	wzr, \[c0, #?-256\]
**	ret
*/
STORE_ZERO_OFFSET (float, m256)

/*
** store_zero_float_m252:
**	str	wzr, \[c0, #?-252\]
**	ret
*/
STORE_ZERO_OFFSET (float, m252)

/*
** store_zero_float_m4:
**	str	wzr, \[c0, #?-4\]
**	ret
*/
STORE_ZERO_OFFSET (float, m4)

/*
** store_zero_float_m1:
**	str	wzr, \[c0, #?-1\]
**	ret
*/
STORE_ZERO_OFFSET (float, m1)

/*
** store_zero_float_1:
**	str	wzr, \[c0, #?1\]
**	ret
*/
STORE_ZERO_OFFSET (float, 1)

/*
** store_zero_float_4:
**	str	wzr, \[c0, #?4\]
**	ret
*/
STORE_ZERO_OFFSET (float, 4)

/*
** store_zero_float_252:
**	str	wzr, \[c0, #?252\]
**	ret
*/
STORE_ZERO_OFFSET (float, 252)

/*
** store_zero_float_255:
**	str	wzr, \[c0, #?255\]
**	ret
*/
STORE_ZERO_OFFSET (float, 255)

/*
** store_zero_float_256:
**	add	(c[0-9]+), c0, #?256
**	str	wzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (float, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_INDEX (float, int32_t, 1)
STORE_ZERO_INDEX (float, uint32_t, 1)
STORE_ZERO_INDEX (float, uint64_t, 1)

STORE_ZERO_INDEX (float, int32_t, 2)
STORE_ZERO_INDEX (float, uint32_t, 2)
STORE_ZERO_INDEX (float, uint64_t, 2)

STORE_ZERO_INDEX (float, int32_t, 4)
STORE_ZERO_INDEX (float, uint32_t, 4)
STORE_ZERO_INDEX (float, uint64_t, 4)

STORE_ZERO_INDEX (float, int32_t, 8)
STORE_ZERO_INDEX (float, uint32_t, 8)
STORE_ZERO_INDEX (float, uint64_t, 8)

STORE_ZERO_INDEX (float, int32_t, 16)
STORE_ZERO_INDEX (float, uint32_t, 16)
STORE_ZERO_INDEX (float, uint64_t, 16)
