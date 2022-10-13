/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

typedef long double tf;

/*
** store_zero_tf_m257:
**	sub	(c[0-9]+), c0, #257
**	str	xzr, \[\1\]
**	str	xzr, \[\1, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (tf, m257)

/*
** store_zero_tf_m256:
**	str	xzr, \[c0, #?-256\]
**	str	xzr, \[c0, #?-248\]
**	ret
*/
STORE_ZERO_OFFSET (tf, m256)

/*
** store_zero_tf_m255:
**	str	xzr, \[c0, #?-255\]
**	str	xzr, \[c0, #?-247\]
**	ret
*/
STORE_ZERO_OFFSET (tf, m255)

/*
** store_zero_tf_m1:
**	str	xzr, \[c0, #?-1\]
**	str	xzr, \[c0, #?7\]
**	ret
*/
STORE_ZERO_OFFSET (tf, m1)

/*
** store_zero_tf_1:
**	str	xzr, \[c0, #?1\]
**	str	xzr, \[c0, #?9\]
**	ret
*/
STORE_ZERO_OFFSET (tf, 1)

/*
** store_zero_tf_247:
**	str	xzr, \[c0, #?247\]
**	str	xzr, \[c0, #?255\]
**	ret
*/
STORE_ZERO_OFFSET (tf, 247)

/*
** store_zero_tf_248:
**	str	xzr, \[c0, #?248\]
**	str	xzr, \[c0, #?256\]
**	ret
*/
STORE_ZERO_OFFSET (tf, 248)

/*
** store_zero_tf_249:
**	add	(c[0-9]+), c0, #?249
**	str	xzr, \[\1\]
**	str	xzr, \[\1, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (tf, 249)

/*
** store_zero_tf_256:
**	add	(c[0-9]+), c0, #?256
**	str	xzr, \[\1\]
**	str	xzr, \[\1, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (tf, 256)

/*
** store_zero_tf_511:
**	add	(c[0-9]+), c0, #?511
**	str	xzr, \[\1\]
**	str	xzr, \[\1, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (tf, 511)

/*
** store_zero_tf_512:
**	add	(c[0-9]+), c0, #?512
**	str	xzr, \[\1\]
**	str	xzr, \[\1, #?8\]
**	ret
*/
STORE_ZERO_OFFSET (tf, 512)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_INDEX (tf, int32_t, 1)
STORE_ZERO_INDEX (tf, uint32_t, 1)
STORE_ZERO_INDEX (tf, uint64_t, 1)

STORE_ZERO_INDEX (tf, int32_t, 2)
STORE_ZERO_INDEX (tf, uint32_t, 2)
STORE_ZERO_INDEX (tf, uint64_t, 2)

STORE_ZERO_INDEX (tf, int32_t, 4)
STORE_ZERO_INDEX (tf, uint32_t, 4)
STORE_ZERO_INDEX (tf, uint64_t, 4)

STORE_ZERO_INDEX (tf, int32_t, 8)
STORE_ZERO_INDEX (tf, uint32_t, 8)
STORE_ZERO_INDEX (tf, uint64_t, 8)

STORE_ZERO_INDEX (tf, int32_t, 16)
STORE_ZERO_INDEX (tf, uint32_t, 16)
STORE_ZERO_INDEX (tf, uint64_t, 16)
