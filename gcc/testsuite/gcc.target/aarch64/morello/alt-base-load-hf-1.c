/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

typedef __fp16 fp16;

/*
** load_w10_fp16_m258:
**	sub	(c[0-9]+), c0, #258
**	ldrh	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, fp16, m258)

/*
** load_w10_fp16_m257:
**	sub	(c[0-9]+), c0, #257
**	ldrh	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, fp16, m257)

/*
** load_w10_fp16_m256:
**	ldrh	w10, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (w10, fp16, m256)

/*
** load_w10_fp16_m254:
**	ldrh	w10, \[c0, #?-254\]
**	ret
*/
LOAD_REG_OFFSET (w10, fp16, m254)

/*
** load_w10_fp16_m2:
**	ldrh	w10, \[c0, #?-2\]
**	ret
*/
LOAD_REG_OFFSET (w10, fp16, m2)

/*
** load_w10_fp16_m1:
**	ldrh	w10, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (w10, fp16, m1)

/*
** load_w10_fp16_1:
**	ldrh	w10, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (w10, fp16, 1)

/*
** load_w10_fp16_2:
**	ldrh	w10, \[c0, #?2\]
**	ret
*/
LOAD_REG_OFFSET (w10, fp16, 2)

/*
** load_w10_fp16_254:
**	ldrh	w10, \[c0, #?254\]
**	ret
*/
LOAD_REG_OFFSET (w10, fp16, 254)

/*
** load_w10_fp16_255:
**	ldrh	w10, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (w10, fp16, 255)

/*
** load_w10_fp16_256:
**	add	(c[0-9]+), c0, #?256
**	ldrh	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, fp16, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (w10, fp16, int32_t, 1)
LOAD_REG_INDEX (w10, fp16, uint32_t, 1)
LOAD_REG_INDEX (w10, fp16, uint64_t, 1)

LOAD_REG_INDEX (w10, fp16, int32_t, 2)
LOAD_REG_INDEX (w10, fp16, uint32_t, 2)
LOAD_REG_INDEX (w10, fp16, uint64_t, 2)

LOAD_REG_INDEX (w10, fp16, int32_t, 4)
LOAD_REG_INDEX (w10, fp16, uint32_t, 4)
LOAD_REG_INDEX (w10, fp16, uint64_t, 4)

LOAD_REG_INDEX (w10, fp16, int32_t, 8)
LOAD_REG_INDEX (w10, fp16, uint32_t, 8)
LOAD_REG_INDEX (w10, fp16, uint64_t, 8)

LOAD_REG_INDEX (w10, fp16, int32_t, 16)
LOAD_REG_INDEX (w10, fp16, uint32_t, 16)
LOAD_REG_INDEX (w10, fp16, uint64_t, 16)

/*
** load_h20_fp16_m258:
**	sub	(c[0-9]+), c0, #258
**	ldr	h20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (h20, fp16, m258)

/*
** load_h20_fp16_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	h20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (h20, fp16, m257)

/*
** load_h20_fp16_m256:
**	ldr	h20, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (h20, fp16, m256)

/*
** load_h20_fp16_m254:
**	ldr	h20, \[c0, #?-254\]
**	ret
*/
LOAD_REG_OFFSET (h20, fp16, m254)

/*
** load_h20_fp16_m2:
**	ldr	h20, \[c0, #?-2\]
**	ret
*/
LOAD_REG_OFFSET (h20, fp16, m2)

/*
** load_h20_fp16_m1:
**	ldr	h20, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (h20, fp16, m1)

/*
** load_h20_fp16_1:
**	ldr	h20, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (h20, fp16, 1)

/*
** load_h20_fp16_2:
**	ldr	h20, \[c0, #?2\]
**	ret
*/
LOAD_REG_OFFSET (h20, fp16, 2)

/*
** load_h20_fp16_254:
**	ldr	h20, \[c0, #?254\]
**	ret
*/
LOAD_REG_OFFSET (h20, fp16, 254)

/*
** load_h20_fp16_255:
**	ldr	h20, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (h20, fp16, 255)

/*
** load_h20_fp16_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	h20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (h20, fp16, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (h20, fp16, int32_t, 1)
LOAD_REG_INDEX (h20, fp16, uint32_t, 1)
LOAD_REG_INDEX (h20, fp16, uint64_t, 1)

LOAD_REG_INDEX (h20, fp16, int32_t, 2)
LOAD_REG_INDEX (h20, fp16, uint32_t, 2)
LOAD_REG_INDEX (h20, fp16, uint64_t, 2)

LOAD_REG_INDEX (h20, fp16, int32_t, 4)
LOAD_REG_INDEX (h20, fp16, uint32_t, 4)
LOAD_REG_INDEX (h20, fp16, uint64_t, 4)

LOAD_REG_INDEX (h20, fp16, int32_t, 8)
LOAD_REG_INDEX (h20, fp16, uint32_t, 8)
LOAD_REG_INDEX (h20, fp16, uint64_t, 8)

LOAD_REG_INDEX (h20, fp16, int32_t, 16)
LOAD_REG_INDEX (h20, fp16, uint32_t, 16)
LOAD_REG_INDEX (h20, fp16, uint64_t, 16)
