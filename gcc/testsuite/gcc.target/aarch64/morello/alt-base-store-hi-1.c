/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** store_w10_uint16_t_m258:
**	sub	(c[0-9]+), c0, #258
**	strh	w10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (w10, uint16_t, m258)

/*
** store_w10_uint16_t_m257:
**	sub	(c[0-9]+), c0, #257
**	strh	w10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (w10, uint16_t, m257)

/*
** store_w10_uint16_t_m256:
**	strh	w10, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (w10, uint16_t, m256)

/*
** store_w10_uint16_t_m254:
**	strh	w10, \[c0, #?-254\]
**	ret
*/
STORE_REG_OFFSET (w10, uint16_t, m254)

/*
** store_w10_uint16_t_m2:
**	strh	w10, \[c0, #?-2\]
**	ret
*/
STORE_REG_OFFSET (w10, uint16_t, m2)

/*
** store_w10_uint16_t_m1:
**	strh	w10, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (w10, uint16_t, m1)

/*
** store_w10_uint16_t_1:
**	strh	w10, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (w10, uint16_t, 1)

/*
** store_w10_uint16_t_2:
**	strh	w10, \[c0, #?2\]
**	ret
*/
STORE_REG_OFFSET (w10, uint16_t, 2)

/*
** store_w10_uint16_t_254:
**	strh	w10, \[c0, #?254\]
**	ret
*/
STORE_REG_OFFSET (w10, uint16_t, 254)

/*
** store_w10_uint16_t_255:
**	strh	w10, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (w10, uint16_t, 255)

/*
** store_w10_uint16_t_256:
**	add	(c[0-9]+), c0, #?256
**	strh	w10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (w10, uint16_t, 256)

/*
** store_w10_uint16_t_int32_t_1:
**	strh	w10, \[c0, w1, sxtw\]
**	ret
*/
STORE_REG_INDEX (w10, uint16_t, int32_t, 1)

/*
** store_w10_uint16_t_uint32_t_1:
**	strh	w10, \[c0, w1, uxtw\]
**	ret
*/
STORE_REG_INDEX (w10, uint16_t, uint32_t, 1)

/*
** store_w10_uint16_t_uint64_t_1:
**	strh	w10, \[c0, x1\]
**	ret
*/
STORE_REG_INDEX (w10, uint16_t, uint64_t, 1)

/*
** store_w10_uint16_t_int32_t_2:
**	strh	w10, \[c0, w1, sxtw #?1\]
**	ret
*/
STORE_REG_INDEX (w10, uint16_t, int32_t, 2)

/*
** store_w10_uint16_t_uint32_t_2:
**	strh	w10, \[c0, w1, uxtw #?1\]
**	ret
*/
STORE_REG_INDEX (w10, uint16_t, uint32_t, 2)

/*
** store_w10_uint16_t_uint64_t_2:
**	strh	w10, \[c0, x1, lsl #?1\]
**	ret
*/
STORE_REG_INDEX (w10, uint16_t, uint64_t, 2)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (w10, uint16_t, int32_t, 4)
STORE_REG_INDEX (w10, uint16_t, uint32_t, 4)
STORE_REG_INDEX (w10, uint16_t, uint64_t, 4)

STORE_REG_INDEX (w10, uint16_t, int32_t, 8)
STORE_REG_INDEX (w10, uint16_t, uint32_t, 8)
STORE_REG_INDEX (w10, uint16_t, uint64_t, 8)

STORE_REG_INDEX (w10, uint16_t, int32_t, 16)
STORE_REG_INDEX (w10, uint16_t, uint32_t, 16)
STORE_REG_INDEX (w10, uint16_t, uint64_t, 16)

/*
** store_h20_uint16_t_m258:
**	sub	(c[0-9]+), c0, #258
**	str	h20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (h20, uint16_t, m258)

/*
** store_h20_uint16_t_m257:
**	sub	(c[0-9]+), c0, #257
**	str	h20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (h20, uint16_t, m257)

/*
** store_h20_uint16_t_m256:
**	str	h20, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (h20, uint16_t, m256)

/*
** store_h20_uint16_t_m254:
**	str	h20, \[c0, #?-254\]
**	ret
*/
STORE_REG_OFFSET (h20, uint16_t, m254)

/*
** store_h20_uint16_t_m2:
**	str	h20, \[c0, #?-2\]
**	ret
*/
STORE_REG_OFFSET (h20, uint16_t, m2)

/*
** store_h20_uint16_t_m1:
**	str	h20, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (h20, uint16_t, m1)

/*
** store_h20_uint16_t_1:
**	str	h20, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (h20, uint16_t, 1)

/*
** store_h20_uint16_t_2:
**	str	h20, \[c0, #?2\]
**	ret
*/
STORE_REG_OFFSET (h20, uint16_t, 2)

/*
** store_h20_uint16_t_254:
**	str	h20, \[c0, #?254\]
**	ret
*/
STORE_REG_OFFSET (h20, uint16_t, 254)

/*
** store_h20_uint16_t_255:
**	str	h20, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (h20, uint16_t, 255)

/*
** store_h20_uint16_t_256:
**	add	(c[0-9]+), c0, #?256
**	str	h20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (h20, uint16_t, 256)

/*
** store_h20_uint16_t_int32_t_1:
**	add	(c[0-9]+), c0, w1, sxtw
**	str	h20, \[\1\]
**	ret
*/
STORE_REG_INDEX (h20, uint16_t, int32_t, 1)

/*
** store_h20_uint16_t_uint32_t_1:
**	add	(c[0-9]+), c0, w1, uxtw
**	str	h20, \[\1\]
**	ret
*/
STORE_REG_INDEX (h20, uint16_t, uint32_t, 1)

/*
** store_h20_uint16_t_uint64_t_1:
**	add	(c[0-9]+), c0, x1
**	str	h20, \[\1\]
**	ret
*/
STORE_REG_INDEX (h20, uint16_t, uint64_t, 1)

/*
** store_h20_uint16_t_int32_t_2:
**	add	(c[0-9]+), c0, w1, sxtw #?1
**	str	h20, \[\1\]
**	ret
*/
STORE_REG_INDEX (h20, uint16_t, int32_t, 2)

/*
** store_h20_uint16_t_uint32_t_2:
**	add	(c[0-9]+), c0, w1, uxtw #?1
**	str	h20, \[\1\]
**	ret
*/
STORE_REG_INDEX (h20, uint16_t, uint32_t, 2)

/*
** store_h20_uint16_t_uint64_t_2:
**	add	(c[0-9]+), c0, x1, lsl #?1
**	str	h20, \[\1\]
**	ret
*/
STORE_REG_INDEX (h20, uint16_t, uint64_t, 2)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (h20, uint16_t, int32_t, 4)
STORE_REG_INDEX (h20, uint16_t, uint32_t, 4)
STORE_REG_INDEX (h20, uint16_t, uint64_t, 4)

STORE_REG_INDEX (h20, uint16_t, int32_t, 8)
STORE_REG_INDEX (h20, uint16_t, uint32_t, 8)
STORE_REG_INDEX (h20, uint16_t, uint64_t, 8)

STORE_REG_INDEX (h20, uint16_t, int32_t, 16)
STORE_REG_INDEX (h20, uint16_t, uint32_t, 16)
STORE_REG_INDEX (h20, uint16_t, uint64_t, 16)
