/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** load_w10_float_m260:
**	sub	(c[0-9]+), c0, #260
**	ldr	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, float, m260)

/*
** load_w10_float_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, float, m257)

/*
** load_w10_float_m256:
**	ldr	w10, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (w10, float, m256)

/*
** load_w10_float_m252:
**	ldr	w10, \[c0, #?-252\]
**	ret
*/
LOAD_REG_OFFSET (w10, float, m252)

/*
** load_w10_float_m4:
**	ldr	w10, \[c0, #?-4\]
**	ret
*/
LOAD_REG_OFFSET (w10, float, m4)

/*
** load_w10_float_m1:
**	ldr	w10, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (w10, float, m1)

/*
** load_w10_float_1:
**	ldr	w10, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (w10, float, 1)

/*
** load_w10_float_4:
**	ldr	w10, \[c0, #?4\]
**	ret
*/
LOAD_REG_OFFSET (w10, float, 4)

/*
** load_w10_float_252:
**	ldr	w10, \[c0, #?252\]
**	ret
*/
LOAD_REG_OFFSET (w10, float, 252)

/*
** load_w10_float_255:
**	ldr	w10, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (w10, float, 255)

/*
** load_w10_float_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, float, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (w10, float, int32_t, 1)
LOAD_REG_INDEX (w10, float, uint32_t, 1)
LOAD_REG_INDEX (w10, float, uint64_t, 1)

LOAD_REG_INDEX (w10, float, int32_t, 2)
LOAD_REG_INDEX (w10, float, uint32_t, 2)
LOAD_REG_INDEX (w10, float, uint64_t, 2)

LOAD_REG_INDEX (w10, float, int32_t, 4)
LOAD_REG_INDEX (w10, float, uint32_t, 4)
LOAD_REG_INDEX (w10, float, uint64_t, 4)

LOAD_REG_INDEX (w10, float, int32_t, 8)
LOAD_REG_INDEX (w10, float, uint32_t, 8)
LOAD_REG_INDEX (w10, float, uint64_t, 8)

LOAD_REG_INDEX (w10, float, int32_t, 16)
LOAD_REG_INDEX (w10, float, uint32_t, 16)
LOAD_REG_INDEX (w10, float, uint64_t, 16)

/*
** load_s20_float_m260:
**	sub	(c[0-9]+), c0, #260
**	ldr	s20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (s20, float, m260)

/*
** load_s20_float_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	s20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (s20, float, m257)

/*
** load_s20_float_m256:
**	ldr	s20, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (s20, float, m256)

/*
** load_s20_float_m252:
**	ldr	s20, \[c0, #?-252\]
**	ret
*/
LOAD_REG_OFFSET (s20, float, m252)

/*
** load_s20_float_m4:
**	ldr	s20, \[c0, #?-4\]
**	ret
*/
LOAD_REG_OFFSET (s20, float, m4)

/*
** load_s20_float_m1:
**	ldr	s20, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (s20, float, m1)

/*
** load_s20_float_1:
**	ldr	s20, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (s20, float, 1)

/*
** load_s20_float_4:
**	ldr	s20, \[c0, #?4\]
**	ret
*/
LOAD_REG_OFFSET (s20, float, 4)

/*
** load_s20_float_252:
**	ldr	s20, \[c0, #?252\]
**	ret
*/
LOAD_REG_OFFSET (s20, float, 252)

/*
** load_s20_float_255:
**	ldr	s20, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (s20, float, 255)

/*
** load_s20_float_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	s20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (s20, float, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (s20, float, int32_t, 1)
LOAD_REG_INDEX (s20, float, uint32_t, 1)
LOAD_REG_INDEX (s20, float, uint64_t, 1)

LOAD_REG_INDEX (s20, float, int32_t, 2)
LOAD_REG_INDEX (s20, float, uint32_t, 2)
LOAD_REG_INDEX (s20, float, uint64_t, 2)

LOAD_REG_INDEX (s20, float, int32_t, 4)
LOAD_REG_INDEX (s20, float, uint32_t, 4)
LOAD_REG_INDEX (s20, float, uint64_t, 4)

LOAD_REG_INDEX (s20, float, int32_t, 8)
LOAD_REG_INDEX (s20, float, uint32_t, 8)
LOAD_REG_INDEX (s20, float, uint64_t, 8)

LOAD_REG_INDEX (s20, float, int32_t, 16)
LOAD_REG_INDEX (s20, float, uint32_t, 16)
LOAD_REG_INDEX (s20, float, uint64_t, 16)
