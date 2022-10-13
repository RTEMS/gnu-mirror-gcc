/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

typedef unsigned int ti __attribute__((mode(TI)));

/*
** store_zero_ti_m257:
**	sub	(c[0-9]+), c0, #257
**	str	xzr, \[\1\]
**	str	xzr, \[\1, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (ti, m257)

/*
** store_zero_ti_m256:
**	str	xzr, \[c0, #?-256\]
**	str	xzr, \[c0, #?-248\]
**	ret
*/
STORE_ZERO_OFFSET (ti, m256)

/*
** store_zero_ti_m255:
**	str	xzr, \[c0, #?-255\]
**	str	xzr, \[c0, #?-247\]
**	ret
*/
STORE_ZERO_OFFSET (ti, m255)

/*
** store_zero_ti_m1:
**	str	xzr, \[c0, #?-1\]
**	str	xzr, \[c0, #?7\]
**	ret
*/
STORE_ZERO_OFFSET (ti, m1)

/*
** store_zero_ti_1:
**	str	xzr, \[c0, #?1\]
**	str	xzr, \[c0, #?9\]
**	ret
*/
STORE_ZERO_OFFSET (ti, 1)

/*
** store_zero_ti_247:
**	str	xzr, \[c0, #?247\]
**	str	xzr, \[c0, #?255\]
**	ret
*/
STORE_ZERO_OFFSET (ti, 247)

/*
** store_zero_ti_248:
**	str	xzr, \[c0, #?248\]
**	str	xzr, \[c0, #?256\]
**	ret
*/
STORE_ZERO_OFFSET (ti, 248)

/*
** store_zero_ti_249:
**	add	(c[0-9]+), c0, #?249
**	str	xzr, \[\1\]
**	str	xzr, \[\1, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (ti, 249)

/*
** store_zero_ti_256:
**	add	(c[0-9]+), c0, #?256
**	str	xzr, \[\1\]
**	str	xzr, \[\1, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (ti, 256)

/*
** store_zero_ti_511:
**	add	(c[0-9]+), c0, #?511
**	str	xzr, \[\1\]
**	str	xzr, \[\1, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (ti, 511)

/*
** store_zero_ti_512:
**	add	(c[0-9]+), c0, #?512
**	str	xzr, \[\1\]
**	str	xzr, \[\1, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (ti, 512)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_INDEX (ti, int32_t, 1)
STORE_ZERO_INDEX (ti, uint32_t, 1)
STORE_ZERO_INDEX (ti, uint64_t, 1)

STORE_ZERO_INDEX (ti, int32_t, 2)
STORE_ZERO_INDEX (ti, uint32_t, 2)
STORE_ZERO_INDEX (ti, uint64_t, 2)

STORE_ZERO_INDEX (ti, int32_t, 4)
STORE_ZERO_INDEX (ti, uint32_t, 4)
STORE_ZERO_INDEX (ti, uint64_t, 4)

STORE_ZERO_INDEX (ti, int32_t, 8)
STORE_ZERO_INDEX (ti, uint32_t, 8)
STORE_ZERO_INDEX (ti, uint64_t, 8)

STORE_ZERO_INDEX (ti, int32_t, 16)
STORE_ZERO_INDEX (ti, uint32_t, 16)
STORE_ZERO_INDEX (ti, uint64_t, 16)
