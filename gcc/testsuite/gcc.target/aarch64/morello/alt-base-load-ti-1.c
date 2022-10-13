/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

typedef unsigned int ti __attribute__((mode(TI)));

/*
** load_x10_ti_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	x10, \[\1\]
**	ldr	x11, \[\1, #?8\]
**	ret
*/
LOAD_REG_OFFSET (x10, ti, m257)

/*
** load_x10_ti_m256:
**	ldr	x10, \[c0, #?-256\]
**	ldr	x11, \[c0, #?-248\]
**	ret
*/
LOAD_REG_OFFSET (x10, ti, m256)

/*
** load_x10_ti_m255:
**	ldr	x10, \[c0, #?-255\]
**	ldr	x11, \[c0, #?-247\]
**	ret
*/
LOAD_REG_OFFSET (x10, ti, m255)

/*
** load_x10_ti_m1:
**	ldr	x10, \[c0, #?-1\]
**	ldr	x11, \[c0, #?7\]
**	ret
*/
LOAD_REG_OFFSET (x10, ti, m1)

/*
** load_x10_ti_1:
**	ldr	x10, \[c0, #?1\]
**	ldr	x11, \[c0, #?9\]
**	ret
*/
LOAD_REG_OFFSET (x10, ti, 1)

/*
** load_x10_ti_247:
**	ldr	x10, \[c0, #?247\]
**	ldr	x11, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (x10, ti, 247)

/*
** load_x10_ti_248:
**	ldr	x10, \[c0, #?248\]
**	ldr	x11, \[c0, #?256\]
**	ret
*/
LOAD_REG_OFFSET (x10, ti, 248)

/*
** load_x10_ti_249:
**	add	(c[0-9]+), c0, #?249
**	ldr	x10, \[\1\]
**	ldr	x11, \[\1, #?8\]
**	ret
*/
LOAD_REG_OFFSET (x10, ti, 249)

/*
** load_x10_ti_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	x10, \[\1\]
**	ldr	x11, \[\1, #?8\]
**	ret
*/
LOAD_REG_OFFSET (x10, ti, 256)

/*
** load_x10_ti_511:
**	add	(c[0-9]+), c0, #?511
**	ldr	x10, \[\1\]
**	ldr	x11, \[\1, #?8\]
**	ret
*/
LOAD_REG_OFFSET (x10, ti, 511)

/*
** load_x10_ti_512:
**	add	(c[0-9]+), c0, #?512
**	ldr	x10, \[\1\]
**	ldr	x11, \[\1, #?8\]
**	ret
*/
LOAD_REG_OFFSET (x10, ti, 512)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (x10, ti, int32_t, 1)
LOAD_REG_INDEX (x10, ti, uint32_t, 1)
LOAD_REG_INDEX (x10, ti, uint64_t, 1)

LOAD_REG_INDEX (x10, ti, int32_t, 2)
LOAD_REG_INDEX (x10, ti, uint32_t, 2)
LOAD_REG_INDEX (x10, ti, uint64_t, 2)

LOAD_REG_INDEX (x10, ti, int32_t, 4)
LOAD_REG_INDEX (x10, ti, uint32_t, 4)
LOAD_REG_INDEX (x10, ti, uint64_t, 4)

LOAD_REG_INDEX (x10, ti, int32_t, 8)
LOAD_REG_INDEX (x10, ti, uint32_t, 8)
LOAD_REG_INDEX (x10, ti, uint64_t, 8)

LOAD_REG_INDEX (x10, ti, int32_t, 16)
LOAD_REG_INDEX (x10, ti, uint32_t, 16)
LOAD_REG_INDEX (x10, ti, uint64_t, 16)

/*
** load_q20_ti_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	q20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (q20, ti, m257)

/*
** load_q20_ti_m256:
**	ldr	q20, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (q20, ti, m256)

/*
** load_q20_ti_m255:
**	ldr	q20, \[c0, #?-255\]
**	ret
*/
LOAD_REG_OFFSET (q20, ti, m255)

/*
** load_q20_ti_m1:
**	ldr	q20, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (q20, ti, m1)

/*
** load_q20_ti_1:
**	ldr	q20, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (q20, ti, 1)

/*
** load_q20_ti_247:
**	ldr	q20, \[c0, #?247\]
**	ret
*/
LOAD_REG_OFFSET (q20, ti, 247)

/*
** load_q20_ti_248:
**	ldr	q20, \[c0, #?248\]
**	ret
*/
LOAD_REG_OFFSET (q20, ti, 248)

/*
** load_q20_ti_249:
**	add	(c[0-9]+), c0, #?249
**	ldr	q20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (q20, ti, 249)

/*
** load_q20_ti_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	q20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (q20, ti, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (q20, ti, int32_t, 1)
LOAD_REG_INDEX (q20, ti, uint32_t, 1)
LOAD_REG_INDEX (q20, ti, uint64_t, 1)

LOAD_REG_INDEX (q20, ti, int32_t, 2)
LOAD_REG_INDEX (q20, ti, uint32_t, 2)
LOAD_REG_INDEX (q20, ti, uint64_t, 2)

LOAD_REG_INDEX (q20, ti, int32_t, 4)
LOAD_REG_INDEX (q20, ti, uint32_t, 4)
LOAD_REG_INDEX (q20, ti, uint64_t, 4)

LOAD_REG_INDEX (q20, ti, int32_t, 8)
LOAD_REG_INDEX (q20, ti, uint32_t, 8)
LOAD_REG_INDEX (q20, ti, uint64_t, 8)

LOAD_REG_INDEX (q20, ti, int32_t, 16)
LOAD_REG_INDEX (q20, ti, uint32_t, 16)
LOAD_REG_INDEX (q20, ti, uint64_t, 16)
