/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-skip-if "" { *-*-* } { "-mfake-capability" } { "" } }  */

#include "load-store-utils.h"

typedef __uintcap_t uintcap_t;

/*
** store_zero_uintcap_t_m272:
** (
**	sub	(c[0-9]+), c0, #272
**	str	czr, \[\1\]
** |
**	sub	(x[0-9]+), x0, #65536
**	str	czr, \[\2, #?65264\]
** )
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, m272)

/*
** store_zero_uintcap_t_m257:
** (
**	sub	(c[0-9]+), c0, #257
**	str	czr, \[\1\]
** |
**	sub	(x[0-9]+), x0, #512
**	str	czr, \[\2, #?255\]
** )
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, m257)

/*
** store_zero_uintcap_t_m256:
**	str	czr, \[[xc]0, #?-256\]
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, m256)

/*
** store_zero_uintcap_t_m255:
**	str	czr, \[[xc]0, #?-255\]
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, m255)

/*
** store_zero_uintcap_t_m16:
**	str	czr, \[[xc]0, #?-16\]
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, m16)

/*
** store_zero_uintcap_t_m1:
**	str	czr, \[[xc]0, #?-1\]
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, m1)

/*
** store_zero_uintcap_t_1:
**	str	czr, \[[xc]0, #?1\]
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, 1)

/*
** store_zero_uintcap_t_16:
**	str	czr, \[[xc]0, #?16\]
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, 16)

/*
** store_zero_uintcap_t_255:
**	str	czr, \[[xc]0, #?255\]
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, 255)

/*
** store_zero_uintcap_t_256:
**	str	czr, \[[xc]0, #?256\]
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, 256)

/*
** store_zero_uintcap_t_257:
** (
**	add	(c[0-9]+), c0, #?256
**	str	czr, \[\1, #?1\]
** |
**	add	(x[0-9]+), x0, #?512
**	str	czr, \[\2, #?-255\]
** )
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, 257)

/*
** store_zero_uintcap_t_65520:
**	str	czr, \[[xc]0, #?65520\]
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, 65520)

/*
** store_zero_uintcap_t_65536:
**	add	([xc][0-9]+), [xc]0, #?65536
**	str	czr, \[\1\]
**	ret
*/
STORE_ZERO_OFFSET (uintcap_t, 65536)

/*
** store_zero_uintcap_t_int32_t_1:
**	str	czr, \[[xc]0, w1, sxtw\]
**	ret
*/
STORE_ZERO_INDEX (uintcap_t, int32_t, 1)

/*
** store_zero_uintcap_t_uint32_t_1:
**	str	czr, \[[xc]0, w1, uxtw\]
**	ret
*/
STORE_ZERO_INDEX (uintcap_t, uint32_t, 1)

/*
** store_zero_uintcap_t_uint64_t_1:
**	str	czr, \[[xc]0, x1\]
**	ret
*/
STORE_ZERO_INDEX (uintcap_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_INDEX (uintcap_t, int32_t, 2)
STORE_ZERO_INDEX (uintcap_t, uint32_t, 2)
STORE_ZERO_INDEX (uintcap_t, uint64_t, 2)

STORE_ZERO_INDEX (uintcap_t, int32_t, 4)
STORE_ZERO_INDEX (uintcap_t, uint32_t, 4)
STORE_ZERO_INDEX (uintcap_t, uint64_t, 4)

STORE_ZERO_INDEX (uintcap_t, int32_t, 8)
STORE_ZERO_INDEX (uintcap_t, uint32_t, 8)
STORE_ZERO_INDEX (uintcap_t, uint64_t, 8)

/*
** store_zero_uintcap_t_int32_t_16:
**	str	czr, \[[xc]0, w1, sxtw #?4\]
**	ret
*/
STORE_ZERO_INDEX (uintcap_t, int32_t, 16)

/*
** store_zero_uintcap_t_uint32_t_16:
**	str	czr, \[[xc]0, w1, uxtw #?4\]
**	ret
*/
STORE_ZERO_INDEX (uintcap_t, uint32_t, 16)

/*
** store_zero_uintcap_t_uint64_t_16:
**	str	czr, \[[xc]0, x1, lsl #?4\]
**	ret
*/
STORE_ZERO_INDEX (uintcap_t, uint64_t, 16)
