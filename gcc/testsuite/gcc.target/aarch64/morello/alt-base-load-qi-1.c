/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** load_w10_uint8_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldrb	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint8_t, m257)

/*
** load_w10_uint8_t_m256:
**	ldrb	w10, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint8_t, m256)

/*
** load_w10_uint8_t_m255:
**	ldrb	w10, \[c0, #?-255\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint8_t, m255)

/*
** load_w10_uint8_t_m1:
**	ldrb	w10, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint8_t, m1)

/*
** load_w10_uint8_t_1:
**	ldrb	w10, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint8_t, 1)

/*
** load_w10_uint8_t_255:
**	ldrb	w10, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint8_t, 255)

/*
** load_w10_uint8_t_256:
**	ldrb	w10, \[c0, #?256\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint8_t, 256)

/*
** load_w10_uint8_t_511:
**	ldrb	w10, \[c0, #?511\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint8_t, 511)

/*
** load_w10_uint8_t_512:
**	add	(c[0-9]+), c0, #?512
**	ldrb	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint8_t, 512)

/*
** load_w10_uint8_t_int32_t_1:
**	ldrb	w10, \[c0, w1, sxtw\]
**	ret
*/
LOAD_REG_INDEX (w10, uint8_t, int32_t, 1)

/*
** load_w10_uint8_t_uint32_t_1:
**	ldrb	w10, \[c0, w1, uxtw\]
**	ret
*/
LOAD_REG_INDEX (w10, uint8_t, uint32_t, 1)

/*
** load_w10_uint8_t_uint64_t_1:
**	ldrb	w10, \[c0, x1\]
**	ret
*/
LOAD_REG_INDEX (w10, uint8_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (w10, uint8_t, int32_t, 2)
LOAD_REG_INDEX (w10, uint8_t, uint32_t, 2)
LOAD_REG_INDEX (w10, uint8_t, uint64_t, 2)

LOAD_REG_INDEX (w10, uint8_t, int32_t, 4)
LOAD_REG_INDEX (w10, uint8_t, uint32_t, 4)
LOAD_REG_INDEX (w10, uint8_t, uint64_t, 4)

LOAD_REG_INDEX (w10, uint8_t, int32_t, 8)
LOAD_REG_INDEX (w10, uint8_t, uint32_t, 8)
LOAD_REG_INDEX (w10, uint8_t, uint64_t, 8)

LOAD_REG_INDEX (w10, uint8_t, int32_t, 16)
LOAD_REG_INDEX (w10, uint8_t, uint32_t, 16)
LOAD_REG_INDEX (w10, uint8_t, uint64_t, 16)

/*
** load_b20_uint8_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	b20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (b20, uint8_t, m257)

/*
** load_b20_uint8_t_m256:
**	ldr	b20, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (b20, uint8_t, m256)

/*
** load_b20_uint8_t_m255:
**	ldr	b20, \[c0, #?-255\]
**	ret
*/
LOAD_REG_OFFSET (b20, uint8_t, m255)

/*
** load_b20_uint8_t_m1:
**	ldr	b20, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (b20, uint8_t, m1)

/*
** load_b20_uint8_t_1:
**	ldr	b20, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (b20, uint8_t, 1)

/*
** load_b20_uint8_t_255:
**	ldr	b20, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (b20, uint8_t, 255)

/*
** load_b20_uint8_t_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	b20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (b20, uint8_t, 256)

/*
** load_b20_uint8_t_int32_t_1:
**	add	(c[0-9]+), c0, w1, sxtw
**	ldr	b20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (b20, uint8_t, int32_t, 1)

/*
** load_b20_uint8_t_uint32_t_1:
**	add	(c[0-9]+), c0, w1, uxtw
**	ldr	b20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (b20, uint8_t, uint32_t, 1)

/*
** load_b20_uint8_t_uint64_t_1:
**	add	(c[0-9]+), c0, x1
**	ldr	b20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (b20, uint8_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (b20, uint8_t, int32_t, 2)
LOAD_REG_INDEX (b20, uint8_t, uint32_t, 2)
LOAD_REG_INDEX (b20, uint8_t, uint64_t, 2)

LOAD_REG_INDEX (b20, uint8_t, int32_t, 4)
LOAD_REG_INDEX (b20, uint8_t, uint32_t, 4)
LOAD_REG_INDEX (b20, uint8_t, uint64_t, 4)

LOAD_REG_INDEX (b20, uint8_t, int32_t, 8)
LOAD_REG_INDEX (b20, uint8_t, uint32_t, 8)
LOAD_REG_INDEX (b20, uint8_t, uint64_t, 8)

LOAD_REG_INDEX (b20, uint8_t, int32_t, 16)
LOAD_REG_INDEX (b20, uint8_t, uint32_t, 16)
LOAD_REG_INDEX (b20, uint8_t, uint64_t, 16)
