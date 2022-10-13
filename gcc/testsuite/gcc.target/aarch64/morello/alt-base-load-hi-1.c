/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** load_w10_uint16_t_m258:
**	sub	(c[0-9]+), c0, #258
**	ldrh	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint16_t, m258)

/*
** load_w10_uint16_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldrh	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint16_t, m257)

/*
** load_w10_uint16_t_m256:
**	ldrh	w10, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint16_t, m256)

/*
** load_w10_uint16_t_m254:
**	ldrh	w10, \[c0, #?-254\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint16_t, m254)

/*
** load_w10_uint16_t_m2:
**	ldrh	w10, \[c0, #?-2\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint16_t, m2)

/*
** load_w10_uint16_t_m1:
**	ldrh	w10, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint16_t, m1)

/*
** load_w10_uint16_t_1:
**	ldrh	w10, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint16_t, 1)

/*
** load_w10_uint16_t_2:
**	ldrh	w10, \[c0, #?2\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint16_t, 2)

/*
** load_w10_uint16_t_254:
**	ldrh	w10, \[c0, #?254\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint16_t, 254)

/*
** load_w10_uint16_t_255:
**	ldrh	w10, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint16_t, 255)

/*
** load_w10_uint16_t_256:
**	add	(c[0-9]+), c0, #?256
**	ldrh	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint16_t, 256)

/*
** load_w10_uint16_t_int32_t_1:
**	ldrh	w10, \[c0, w1, sxtw\]
**	ret
*/
LOAD_REG_INDEX (w10, uint16_t, int32_t, 1)

/*
** load_w10_uint16_t_uint32_t_1:
**	ldrh	w10, \[c0, w1, uxtw\]
**	ret
*/
LOAD_REG_INDEX (w10, uint16_t, uint32_t, 1)

/*
** load_w10_uint16_t_uint64_t_1:
**	ldrh	w10, \[c0, x1\]
**	ret
*/
LOAD_REG_INDEX (w10, uint16_t, uint64_t, 1)

/*
** load_w10_uint16_t_int32_t_2:
**	ldrh	w10, \[c0, w1, sxtw #?1\]
**	ret
*/
LOAD_REG_INDEX (w10, uint16_t, int32_t, 2)

/*
** load_w10_uint16_t_uint32_t_2:
**	ldrh	w10, \[c0, w1, uxtw #?1\]
**	ret
*/
LOAD_REG_INDEX (w10, uint16_t, uint32_t, 2)

/*
** load_w10_uint16_t_uint64_t_2:
**	ldrh	w10, \[c0, x1, lsl #?1\]
**	ret
*/
LOAD_REG_INDEX (w10, uint16_t, uint64_t, 2)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (w10, uint16_t, int32_t, 4)
LOAD_REG_INDEX (w10, uint16_t, uint32_t, 4)
LOAD_REG_INDEX (w10, uint16_t, uint64_t, 4)

LOAD_REG_INDEX (w10, uint16_t, int32_t, 8)
LOAD_REG_INDEX (w10, uint16_t, uint32_t, 8)
LOAD_REG_INDEX (w10, uint16_t, uint64_t, 8)

LOAD_REG_INDEX (w10, uint16_t, int32_t, 16)
LOAD_REG_INDEX (w10, uint16_t, uint32_t, 16)
LOAD_REG_INDEX (w10, uint16_t, uint64_t, 16)

/*
** load_h20_uint16_t_m258:
**	sub	(c[0-9]+), c0, #258
**	ldr	h20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (h20, uint16_t, m258)

/*
** load_h20_uint16_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	h20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (h20, uint16_t, m257)

/*
** load_h20_uint16_t_m256:
**	ldr	h20, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (h20, uint16_t, m256)

/*
** load_h20_uint16_t_m254:
**	ldr	h20, \[c0, #?-254\]
**	ret
*/
LOAD_REG_OFFSET (h20, uint16_t, m254)

/*
** load_h20_uint16_t_m2:
**	ldr	h20, \[c0, #?-2\]
**	ret
*/
LOAD_REG_OFFSET (h20, uint16_t, m2)

/*
** load_h20_uint16_t_m1:
**	ldr	h20, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (h20, uint16_t, m1)

/*
** load_h20_uint16_t_1:
**	ldr	h20, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (h20, uint16_t, 1)

/*
** load_h20_uint16_t_2:
**	ldr	h20, \[c0, #?2\]
**	ret
*/
LOAD_REG_OFFSET (h20, uint16_t, 2)

/*
** load_h20_uint16_t_254:
**	ldr	h20, \[c0, #?254\]
**	ret
*/
LOAD_REG_OFFSET (h20, uint16_t, 254)

/*
** load_h20_uint16_t_255:
**	ldr	h20, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (h20, uint16_t, 255)

/*
** load_h20_uint16_t_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	h20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (h20, uint16_t, 256)

/*
** load_h20_uint16_t_int32_t_1:
**	add	(c[0-9]+), c0, w1, sxtw
**	ldr	h20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (h20, uint16_t, int32_t, 1)

/*
** load_h20_uint16_t_uint32_t_1:
**	add	(c[0-9]+), c0, w1, uxtw
**	ldr	h20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (h20, uint16_t, uint32_t, 1)

/*
** load_h20_uint16_t_uint64_t_1:
**	add	(c[0-9]+), c0, x1
**	ldr	h20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (h20, uint16_t, uint64_t, 1)

/*
** load_h20_uint16_t_int32_t_2:
**	add	(c[0-9]+), c0, w1, sxtw #?1
**	ldr	h20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (h20, uint16_t, int32_t, 2)

/*
** load_h20_uint16_t_uint32_t_2:
**	add	(c[0-9]+), c0, w1, uxtw #?1
**	ldr	h20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (h20, uint16_t, uint32_t, 2)

/*
** load_h20_uint16_t_uint64_t_2:
**	add	(c[0-9]+), c0, x1, lsl #?1
**	ldr	h20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (h20, uint16_t, uint64_t, 2)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (h20, uint16_t, int32_t, 4)
LOAD_REG_INDEX (h20, uint16_t, uint32_t, 4)
LOAD_REG_INDEX (h20, uint16_t, uint64_t, 4)

LOAD_REG_INDEX (h20, uint16_t, int32_t, 8)
LOAD_REG_INDEX (h20, uint16_t, uint32_t, 8)
LOAD_REG_INDEX (h20, uint16_t, uint64_t, 8)

LOAD_REG_INDEX (h20, uint16_t, int32_t, 16)
LOAD_REG_INDEX (h20, uint16_t, uint32_t, 16)
LOAD_REG_INDEX (h20, uint16_t, uint64_t, 16)
