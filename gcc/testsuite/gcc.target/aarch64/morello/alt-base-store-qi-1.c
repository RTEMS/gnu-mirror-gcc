/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** store_w10_uint8_t_m257:
**	sub	(c[0-9]+), c0, #257
**	strb	w10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (w10, uint8_t, m257)

/*
** store_w10_uint8_t_m256:
**	strb	w10, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (w10, uint8_t, m256)

/*
** store_w10_uint8_t_m255:
**	strb	w10, \[c0, #?-255\]
**	ret
*/
STORE_REG_OFFSET (w10, uint8_t, m255)

/*
** store_w10_uint8_t_m1:
**	strb	w10, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (w10, uint8_t, m1)

/*
** store_w10_uint8_t_1:
**	strb	w10, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (w10, uint8_t, 1)

/*
** store_w10_uint8_t_255:
**	strb	w10, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (w10, uint8_t, 255)

/*
** store_w10_uint8_t_256:
**	strb	w10, \[c0, #?256\]
**	ret
*/
STORE_REG_OFFSET (w10, uint8_t, 256)

/*
** store_w10_uint8_t_511:
**	strb	w10, \[c0, #?511\]
**	ret
*/
STORE_REG_OFFSET (w10, uint8_t, 511)

/*
** store_w10_uint8_t_512:
**	add	(c[0-9]+), c0, #?512
**	strb	w10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (w10, uint8_t, 512)

/*
** store_w10_uint8_t_int32_t_1:
**	strb	w10, \[c0, w1, sxtw\]
**	ret
*/
STORE_REG_INDEX (w10, uint8_t, int32_t, 1)

/*
** store_w10_uint8_t_uint32_t_1:
**	strb	w10, \[c0, w1, uxtw\]
**	ret
*/
STORE_REG_INDEX (w10, uint8_t, uint32_t, 1)

/*
** store_w10_uint8_t_uint64_t_1:
**	strb	w10, \[c0, x1\]
**	ret
*/
STORE_REG_INDEX (w10, uint8_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (w10, uint8_t, int32_t, 2)
STORE_REG_INDEX (w10, uint8_t, uint32_t, 2)
STORE_REG_INDEX (w10, uint8_t, uint64_t, 2)

STORE_REG_INDEX (w10, uint8_t, int32_t, 4)
STORE_REG_INDEX (w10, uint8_t, uint32_t, 4)
STORE_REG_INDEX (w10, uint8_t, uint64_t, 4)

STORE_REG_INDEX (w10, uint8_t, int32_t, 8)
STORE_REG_INDEX (w10, uint8_t, uint32_t, 8)
STORE_REG_INDEX (w10, uint8_t, uint64_t, 8)

STORE_REG_INDEX (w10, uint8_t, int32_t, 16)
STORE_REG_INDEX (w10, uint8_t, uint32_t, 16)
STORE_REG_INDEX (w10, uint8_t, uint64_t, 16)

/*
** store_b20_uint8_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	b20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (b20, uint8_t, m257)

/*
** store_b20_uint8_t_m256:
**	str	b20, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (b20, uint8_t, m256)

/*
** store_b20_uint8_t_m255:
**	str	b20, \[c0, #?-255\]
**	ret
*/
STORE_REG_OFFSET (b20, uint8_t, m255)

/*
** store_b20_uint8_t_m1:
**	str	b20, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (b20, uint8_t, m1)

/*
** store_b20_uint8_t_1:
**	str	b20, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (b20, uint8_t, 1)

/*
** store_b20_uint8_t_255:
**	str	b20, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (b20, uint8_t, 255)

/*
** store_b20_uint8_t_256:
**	add	(c[0-9]+), c0, #?256
**	str	b20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (b20, uint8_t, 256)

/*
** store_b20_uint8_t_int32_t_1:
**	add	(c[0-9]+), c0, w1, sxtw
**	str	b20, \[\1\]
**	ret
*/
STORE_REG_INDEX (b20, uint8_t, int32_t, 1)

/*
** store_b20_uint8_t_uint32_t_1:
**	add	(c[0-9]+), c0, w1, uxtw
**	str	b20, \[\1\]
**	ret
*/
STORE_REG_INDEX (b20, uint8_t, uint32_t, 1)

/*
** store_b20_uint8_t_uint64_t_1:
**	add	(c[0-9]+), c0, x1
**	str	b20, \[\1\]
**	ret
*/
STORE_REG_INDEX (b20, uint8_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (b20, uint8_t, int32_t, 2)
STORE_REG_INDEX (b20, uint8_t, uint32_t, 2)
STORE_REG_INDEX (b20, uint8_t, uint64_t, 2)

STORE_REG_INDEX (b20, uint8_t, int32_t, 4)
STORE_REG_INDEX (b20, uint8_t, uint32_t, 4)
STORE_REG_INDEX (b20, uint8_t, uint64_t, 4)

STORE_REG_INDEX (b20, uint8_t, int32_t, 8)
STORE_REG_INDEX (b20, uint8_t, uint32_t, 8)
STORE_REG_INDEX (b20, uint8_t, uint64_t, 8)

STORE_REG_INDEX (b20, uint8_t, int32_t, 16)
STORE_REG_INDEX (b20, uint8_t, uint32_t, 16)
STORE_REG_INDEX (b20, uint8_t, uint64_t, 16)
