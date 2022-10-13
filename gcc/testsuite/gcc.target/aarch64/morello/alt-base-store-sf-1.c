/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** store_w10_float_m260:
**	sub	(c[0-9]+), c0, #260
**	str	w10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (w10, float, m260)

/*
** store_w10_float_m257:
**	sub	(c[0-9]+), c0, #257
**	str	w10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (w10, float, m257)

/*
** store_w10_float_m256:
**	str	w10, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (w10, float, m256)

/*
** store_w10_float_m252:
**	str	w10, \[c0, #?-252\]
**	ret
*/
STORE_REG_OFFSET (w10, float, m252)

/*
** store_w10_float_m4:
**	str	w10, \[c0, #?-4\]
**	ret
*/
STORE_REG_OFFSET (w10, float, m4)

/*
** store_w10_float_m1:
**	str	w10, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (w10, float, m1)

/*
** store_w10_float_1:
**	str	w10, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (w10, float, 1)

/*
** store_w10_float_4:
**	str	w10, \[c0, #?4\]
**	ret
*/
STORE_REG_OFFSET (w10, float, 4)

/*
** store_w10_float_252:
**	str	w10, \[c0, #?252\]
**	ret
*/
STORE_REG_OFFSET (w10, float, 252)

/*
** store_w10_float_255:
**	str	w10, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (w10, float, 255)

/*
** store_w10_float_256:
**	add	(c[0-9]+), c0, #?256
**	str	w10, \[\1\]
**	ret
*/
STORE_REG_OFFSET (w10, float, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (w10, float, int32_t, 1)
STORE_REG_INDEX (w10, float, uint32_t, 1)
STORE_REG_INDEX (w10, float, uint64_t, 1)

STORE_REG_INDEX (w10, float, int32_t, 2)
STORE_REG_INDEX (w10, float, uint32_t, 2)
STORE_REG_INDEX (w10, float, uint64_t, 2)

STORE_REG_INDEX (w10, float, int32_t, 4)
STORE_REG_INDEX (w10, float, uint32_t, 4)
STORE_REG_INDEX (w10, float, uint64_t, 4)

STORE_REG_INDEX (w10, float, int32_t, 8)
STORE_REG_INDEX (w10, float, uint32_t, 8)
STORE_REG_INDEX (w10, float, uint64_t, 8)

STORE_REG_INDEX (w10, float, int32_t, 16)
STORE_REG_INDEX (w10, float, uint32_t, 16)
STORE_REG_INDEX (w10, float, uint64_t, 16)

/*
** store_s20_float_m260:
**	sub	(c[0-9]+), c0, #260
**	str	s20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (s20, float, m260)

/*
** store_s20_float_m257:
**	sub	(c[0-9]+), c0, #257
**	str	s20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (s20, float, m257)

/*
** store_s20_float_m256:
**	str	s20, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (s20, float, m256)

/*
** store_s20_float_m252:
**	str	s20, \[c0, #?-252\]
**	ret
*/
STORE_REG_OFFSET (s20, float, m252)

/*
** store_s20_float_m4:
**	str	s20, \[c0, #?-4\]
**	ret
*/
STORE_REG_OFFSET (s20, float, m4)

/*
** store_s20_float_m1:
**	str	s20, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (s20, float, m1)

/*
** store_s20_float_1:
**	str	s20, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (s20, float, 1)

/*
** store_s20_float_4:
**	str	s20, \[c0, #?4\]
**	ret
*/
STORE_REG_OFFSET (s20, float, 4)

/*
** store_s20_float_252:
**	str	s20, \[c0, #?252\]
**	ret
*/
STORE_REG_OFFSET (s20, float, 252)

/*
** store_s20_float_255:
**	str	s20, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (s20, float, 255)

/*
** store_s20_float_256:
**	add	(c[0-9]+), c0, #?256
**	str	s20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (s20, float, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (s20, float, int32_t, 1)
STORE_REG_INDEX (s20, float, uint32_t, 1)
STORE_REG_INDEX (s20, float, uint64_t, 1)

STORE_REG_INDEX (s20, float, int32_t, 2)
STORE_REG_INDEX (s20, float, uint32_t, 2)
STORE_REG_INDEX (s20, float, uint64_t, 2)

STORE_REG_INDEX (s20, float, int32_t, 4)
STORE_REG_INDEX (s20, float, uint32_t, 4)
STORE_REG_INDEX (s20, float, uint64_t, 4)

STORE_REG_INDEX (s20, float, int32_t, 8)
STORE_REG_INDEX (s20, float, uint32_t, 8)
STORE_REG_INDEX (s20, float, uint64_t, 8)

STORE_REG_INDEX (s20, float, int32_t, 16)
STORE_REG_INDEX (s20, float, uint32_t, 16)
STORE_REG_INDEX (s20, float, uint64_t, 16)
