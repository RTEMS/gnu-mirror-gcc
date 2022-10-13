/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

typedef long double tf;

/*
** store_x10_tf_m257:
**	sub	(c[0-9]+), c0, #257
**	str	x10, \[\1\]
**	str	x11, \[\1, #?8\]
**	ret
*/
STORE_REG_OFFSET (x10, tf, m257)

/*
** store_x10_tf_m256:
**	str	x10, \[c0, #?-256\]
**	str	x11, \[c0, #?-248\]
**	ret
*/
STORE_REG_OFFSET (x10, tf, m256)

/*
** store_x10_tf_m255:
**	str	x10, \[c0, #?-255\]
**	str	x11, \[c0, #?-247\]
**	ret
*/
STORE_REG_OFFSET (x10, tf, m255)

/*
** store_x10_tf_m1:
**	str	x10, \[c0, #?-1\]
**	str	x11, \[c0, #?7\]
**	ret
*/
STORE_REG_OFFSET (x10, tf, m1)

/*
** store_x10_tf_1:
**	str	x10, \[c0, #?1\]
**	str	x11, \[c0, #?9\]
**	ret
*/
STORE_REG_OFFSET (x10, tf, 1)

/*
** store_x10_tf_247:
**	str	x10, \[c0, #?247\]
**	str	x11, \[c0, #?255\]
**	ret
*/
STORE_REG_OFFSET (x10, tf, 247)

/*
** store_x10_tf_248:
**	str	x10, \[c0, #?248\]
**	str	x11, \[c0, #?256\]
**	ret
*/
STORE_REG_OFFSET (x10, tf, 248)

/*
** store_x10_tf_249:
**	add	(c[0-9]+), c0, #?249
**	str	x10, \[\1\]
**	str	x11, \[\1, #?8\]
**	ret
*/
STORE_REG_OFFSET (x10, tf, 249)

/*
** store_x10_tf_256:
**	add	(c[0-9]+), c0, #?256
**	str	x10, \[\1\]
**	str	x11, \[\1, #?8\]
**	ret
*/
STORE_REG_OFFSET (x10, tf, 256)

/*
** store_x10_tf_511:
**	add	(c[0-9]+), c0, #?511
**	str	x10, \[\1\]
**	str	x11, \[\1, #?8\]
**	ret
*/
STORE_REG_OFFSET (x10, tf, 511)

/*
** store_x10_tf_512:
**	add	(c[0-9]+), c0, #?512
**	str	x10, \[\1\]
**	str	x11, \[\1, #?8\]
**	ret
*/
STORE_REG_OFFSET (x10, tf, 512)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (x10, tf, int32_t, 1)
STORE_REG_INDEX (x10, tf, uint32_t, 1)
STORE_REG_INDEX (x10, tf, uint64_t, 1)

STORE_REG_INDEX (x10, tf, int32_t, 2)
STORE_REG_INDEX (x10, tf, uint32_t, 2)
STORE_REG_INDEX (x10, tf, uint64_t, 2)

STORE_REG_INDEX (x10, tf, int32_t, 4)
STORE_REG_INDEX (x10, tf, uint32_t, 4)
STORE_REG_INDEX (x10, tf, uint64_t, 4)

STORE_REG_INDEX (x10, tf, int32_t, 8)
STORE_REG_INDEX (x10, tf, uint32_t, 8)
STORE_REG_INDEX (x10, tf, uint64_t, 8)

STORE_REG_INDEX (x10, tf, int32_t, 16)
STORE_REG_INDEX (x10, tf, uint32_t, 16)
STORE_REG_INDEX (x10, tf, uint64_t, 16)

/*
** store_q20_tf_m257:
**	sub	(c[0-9]+), c0, #257
**	str	q20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (q20, tf, m257)

/*
** store_q20_tf_m256:
**	str	q20, \[c0, #?-256\]
**	ret
*/
STORE_REG_OFFSET (q20, tf, m256)

/*
** store_q20_tf_m255:
**	str	q20, \[c0, #?-255\]
**	ret
*/
STORE_REG_OFFSET (q20, tf, m255)

/*
** store_q20_tf_m1:
**	str	q20, \[c0, #?-1\]
**	ret
*/
STORE_REG_OFFSET (q20, tf, m1)

/*
** store_q20_tf_1:
**	str	q20, \[c0, #?1\]
**	ret
*/
STORE_REG_OFFSET (q20, tf, 1)

/*
** store_q20_tf_247:
**	str	q20, \[c0, #?247\]
**	ret
*/
STORE_REG_OFFSET (q20, tf, 247)

/*
** store_q20_tf_248:
**	str	q20, \[c0, #?248\]
**	ret
*/
STORE_REG_OFFSET (q20, tf, 248)

/*
** store_q20_tf_249:
**	add	(c[0-9]+), c0, #?249
**	str	q20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (q20, tf, 249)

/*
** store_q20_tf_256:
**	add	(c[0-9]+), c0, #?256
**	str	q20, \[\1\]
**	ret
*/
STORE_REG_OFFSET (q20, tf, 256)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_REG_INDEX (q20, tf, int32_t, 1)
STORE_REG_INDEX (q20, tf, uint32_t, 1)
STORE_REG_INDEX (q20, tf, uint64_t, 1)

STORE_REG_INDEX (q20, tf, int32_t, 2)
STORE_REG_INDEX (q20, tf, uint32_t, 2)
STORE_REG_INDEX (q20, tf, uint64_t, 2)

STORE_REG_INDEX (q20, tf, int32_t, 4)
STORE_REG_INDEX (q20, tf, uint32_t, 4)
STORE_REG_INDEX (q20, tf, uint64_t, 4)

STORE_REG_INDEX (q20, tf, int32_t, 8)
STORE_REG_INDEX (q20, tf, uint32_t, 8)
STORE_REG_INDEX (q20, tf, uint64_t, 8)

STORE_REG_INDEX (q20, tf, int32_t, 16)
STORE_REG_INDEX (q20, tf, uint32_t, 16)
STORE_REG_INDEX (q20, tf, uint64_t, 16)
