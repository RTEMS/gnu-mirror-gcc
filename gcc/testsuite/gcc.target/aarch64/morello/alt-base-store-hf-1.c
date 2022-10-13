/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

typedef __fp16 fp16;

/*
** store_w10_fp16_m258:
**	sub	(c[0-9]+), c0, #258
**	strh	w10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (w10, fp16, m258)

/*
** store_w10_fp16_m257:
**	sub	(c[0-9]+), c0, #257
**	strh	w10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (w10, fp16, m257)

/*
** store_w10_fp16_m256:
**	strh	w10, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (w10, fp16, m256)

/*
** store_w10_fp16_m254:
**	strh	w10, \[c0, #?-254\]
**	ret
*/
STORE_REG_OFFSET (w10, fp16, m254)

/*
** store_w10_fp16_m2:
**	strh	w10, \[c0, #?-2\]
**	ret
*/
STORE_REG_OFFSET (w10, fp16, m2)

/*
** store_w10_fp16_m1:
**	strh	w10, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (w10, fp16, m1)

/*
** store_w10_fp16_1:
**	strh	w10, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (w10, fp16, 1)

/*
** store_w10_fp16_2:
**	strh	w10, \[c0, #?2\]
**	ret
*/
STORE_REG_OFFSET (w10, fp16, 2)

/*
** store_w10_fp16_254:
**	strh	w10, \[c0, #?254\]
**	ret
*/
STORE_REG_OFFSET (w10, fp16, 254)

/*
** store_w10_fp16_255:
**	strh	w10, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (w10, fp16, 255)

/*
** store_w10_fp16_256:
**	add	(c[0-9]+), c0, #?256
**	strh	w10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (w10, fp16, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (w10, fp16, int32_t, 1)
STORE_REG_INDEX (w10, fp16, uint32_t, 1)
STORE_REG_INDEX (w10, fp16, uint64_t, 1)

STORE_REG_INDEX (w10, fp16, int32_t, 2)
STORE_REG_INDEX (w10, fp16, uint32_t, 2)
STORE_REG_INDEX (w10, fp16, uint64_t, 2)

STORE_REG_INDEX (w10, fp16, int32_t, 4)
STORE_REG_INDEX (w10, fp16, uint32_t, 4)
STORE_REG_INDEX (w10, fp16, uint64_t, 4)

STORE_REG_INDEX (w10, fp16, int32_t, 8)
STORE_REG_INDEX (w10, fp16, uint32_t, 8)
STORE_REG_INDEX (w10, fp16, uint64_t, 8)

STORE_REG_INDEX (w10, fp16, int32_t, 16)
STORE_REG_INDEX (w10, fp16, uint32_t, 16)
STORE_REG_INDEX (w10, fp16, uint64_t, 16)

/*
** store_h20_fp16_m258:
**	sub	(c[0-9]+), c0, #258
**	str	h20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (h20, fp16, m258)

/*
** store_h20_fp16_m257:
**	sub	(c[0-9]+), c0, #257
**	str	h20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (h20, fp16, m257)

/*
** store_h20_fp16_m256:
**	str	h20, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (h20, fp16, m256)

/*
** store_h20_fp16_m254:
**	str	h20, \[c0, #?-254\]
**	ret
*/
STORE_REG_OFFSET (h20, fp16, m254)

/*
** store_h20_fp16_m2:
**	str	h20, \[c0, #?-2\]
**	ret
*/
STORE_REG_OFFSET (h20, fp16, m2)

/*
** store_h20_fp16_m1:
**	str	h20, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (h20, fp16, m1)

/*
** store_h20_fp16_1:
**	str	h20, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (h20, fp16, 1)

/*
** store_h20_fp16_2:
**	str	h20, \[c0, #?2\]
**	ret
*/
STORE_REG_OFFSET (h20, fp16, 2)

/*
** store_h20_fp16_254:
**	str	h20, \[c0, #?254\]
**	ret
*/
STORE_REG_OFFSET (h20, fp16, 254)

/*
** store_h20_fp16_255:
**	str	h20, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (h20, fp16, 255)

/*
** store_h20_fp16_256:
**	add	(c[0-9]+), c0, #?256
**	str	h20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (h20, fp16, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (h20, fp16, int32_t, 1)
STORE_REG_INDEX (h20, fp16, uint32_t, 1)
STORE_REG_INDEX (h20, fp16, uint64_t, 1)

STORE_REG_INDEX (h20, fp16, int32_t, 2)
STORE_REG_INDEX (h20, fp16, uint32_t, 2)
STORE_REG_INDEX (h20, fp16, uint64_t, 2)

STORE_REG_INDEX (h20, fp16, int32_t, 4)
STORE_REG_INDEX (h20, fp16, uint32_t, 4)
STORE_REG_INDEX (h20, fp16, uint64_t, 4)

STORE_REG_INDEX (h20, fp16, int32_t, 8)
STORE_REG_INDEX (h20, fp16, uint32_t, 8)
STORE_REG_INDEX (h20, fp16, uint64_t, 8)

STORE_REG_INDEX (h20, fp16, int32_t, 16)
STORE_REG_INDEX (h20, fp16, uint32_t, 16)
STORE_REG_INDEX (h20, fp16, uint64_t, 16)
