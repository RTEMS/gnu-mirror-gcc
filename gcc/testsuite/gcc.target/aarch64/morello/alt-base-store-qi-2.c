/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** store_zero_uint8_t_m257:
**	sub	(c[0-9]+), c0, #257
**	strb	wzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (uint8_t, m257)

/*
** store_zero_uint8_t_m256:
**	strb	wzr, \[c0, #?-256\]
**	ret
*/
STORE_ZERO_OFFSET (uint8_t, m256)

/*
** store_zero_uint8_t_m255:
**	strb	wzr, \[c0, #?-255\]
**	ret
*/
STORE_ZERO_OFFSET (uint8_t, m255)

/*
** store_zero_uint8_t_m1:
**	strb	wzr, \[c0, #?-1\]
**	ret
*/
STORE_ZERO_OFFSET (uint8_t, m1)

/*
** store_zero_uint8_t_1:
**	strb	wzr, \[c0, #?1\]
**	ret
*/
STORE_ZERO_OFFSET (uint8_t, 1)

/*
** store_zero_uint8_t_255:
**	strb	wzr, \[c0, #?255\]
**	ret
*/
STORE_ZERO_OFFSET (uint8_t, 255)

/*
** store_zero_uint8_t_256:
**	strb	wzr, \[c0, #?256\]
**	ret
*/
STORE_ZERO_OFFSET (uint8_t, 256)

/*
** store_zero_uint8_t_511:
**	strb	wzr, \[c0, #?511\]
**	ret
*/
STORE_ZERO_OFFSET (uint8_t, 511)

/*
** store_zero_uint8_t_512:
**	add	(c[0-9]+), c0, #?512
**	strb	wzr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (uint8_t, 512)

/*
** store_zero_uint8_t_int32_t_1:
**	strb	wzr, \[c0, w1, sxtw\]
**	ret
*/
STORE_ZERO_INDEX (uint8_t, int32_t, 1)

/*
** store_zero_uint8_t_uint32_t_1:
**	strb	wzr, \[c0, w1, uxtw\]
**	ret
*/
STORE_ZERO_INDEX (uint8_t, uint32_t, 1)

/*
** store_zero_uint8_t_uint64_t_1:
**	strb	wzr, \[c0, x1\]
**	ret
*/
STORE_ZERO_INDEX (uint8_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_INDEX (uint8_t, int32_t, 2)
STORE_ZERO_INDEX (uint8_t, uint32_t, 2)
STORE_ZERO_INDEX (uint8_t, uint64_t, 2)

STORE_ZERO_INDEX (uint8_t, int32_t, 4)
STORE_ZERO_INDEX (uint8_t, uint32_t, 4)
STORE_ZERO_INDEX (uint8_t, uint64_t, 4)

STORE_ZERO_INDEX (uint8_t, int32_t, 8)
STORE_ZERO_INDEX (uint8_t, uint32_t, 8)
STORE_ZERO_INDEX (uint8_t, uint64_t, 8)

STORE_ZERO_INDEX (uint8_t, int32_t, 16)
STORE_ZERO_INDEX (uint8_t, uint32_t, 16)
STORE_ZERO_INDEX (uint8_t, uint64_t, 16)
