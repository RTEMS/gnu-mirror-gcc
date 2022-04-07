/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-skip-if "" { *-*-* } { "-mfake-capability" } { "" } }  */

#include "load-store-utils.h"

typedef __uintcap_t uintcap_t;

/*
** load_c10_uintcap_t_m272:
** (
**	sub	(c[0-9]+), c0, #272
**	ldr	c10, \[\1\]
** |
**	sub	(x[0-9]+), x0, #65536
**	ldr	c10, \[\2, #?65264\]
** )
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, m272)

/*
** load_c10_uintcap_t_m257:
** (
**	sub	(c[0-9]+), c0, #257
**	ldr	c10, \[\1\]
** |
**	sub	(x[0-9]+), x0, #512
**	ldr	c10, \[\2, #?255\]
** )
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, m257)

/*
** load_c10_uintcap_t_m256:
**	ldr	c10, \[[cx]0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, m256)

/*
** load_c10_uintcap_t_m255:
**	ldr	c10, \[[cx]0, #?-255\]
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, m255)

/*
** load_c10_uintcap_t_m16:
**	ldr	c10, \[[cx]0, #?-16\]
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, m16)

/*
** load_c10_uintcap_t_m1:
**	ldr	c10, \[[cx]0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, m1)

/*
** load_c10_uintcap_t_1:
**	ldr	c10, \[[cx]0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, 1)

/*
** load_c10_uintcap_t_16:
**	ldr	c10, \[[cx]0, #?16\]
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, 16)

/*
** load_c10_uintcap_t_255:
**	ldr	c10, \[[cx]0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, 255)

/*
** load_c10_uintcap_t_256:
**	ldr	c10, \[[cx]0, #?256\]
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, 256)

/*
** load_c10_uintcap_t_257:
** (
**	add	(c[0-9]+), c0, #?256
**	ldr	c10, \[\1, #?1\]
** |
**	add	(x[0-9]+), x0, #?512
**	ldr	c10, \[\2, #?-255\]
** )
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, 257)

/*
** load_c10_uintcap_t_65520:
**	ldr	c10, \[[cx]0, #?65520\]
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, 65520)

/*
** load_c10_uintcap_t_65536:
**	add	([xc][0-9]+), [cx]0, #?65536
**	ldr	c10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (c10, uintcap_t, 65536)

/*
** load_c10_uintcap_t_int32_t_1:
**	ldr	c10, \[[cx]0, w1, sxtw\]
**	ret
*/
LOAD_REG_INDEX (c10, uintcap_t, int32_t, 1)

/*
** load_c10_uintcap_t_uint32_t_1:
**	ldr	c10, \[[cx]0, w1, uxtw\]
**	ret
*/
LOAD_REG_INDEX (c10, uintcap_t, uint32_t, 1)

/*
** load_c10_uintcap_t_uint64_t_1:
**	ldr	c10, \[[cx]0, x1\]
**	ret
*/
LOAD_REG_INDEX (c10, uintcap_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (c10, uintcap_t, int32_t, 2)
LOAD_REG_INDEX (c10, uintcap_t, uint32_t, 2)
LOAD_REG_INDEX (c10, uintcap_t, uint64_t, 2)

LOAD_REG_INDEX (c10, uintcap_t, int32_t, 4)
LOAD_REG_INDEX (c10, uintcap_t, uint32_t, 4)
LOAD_REG_INDEX (c10, uintcap_t, uint64_t, 4)

LOAD_REG_INDEX (c10, uintcap_t, int32_t, 8)
LOAD_REG_INDEX (c10, uintcap_t, uint32_t, 8)
LOAD_REG_INDEX (c10, uintcap_t, uint64_t, 8)

/*
** load_c10_uintcap_t_int32_t_16:
**	ldr	c10, \[[cx]0, w1, sxtw #?4\]
**	ret
*/
LOAD_REG_INDEX (c10, uintcap_t, int32_t, 16)

/*
** load_c10_uintcap_t_uint32_t_16:
**	ldr	c10, \[[cx]0, w1, uxtw #?4\]
**	ret
*/
LOAD_REG_INDEX (c10, uintcap_t, uint32_t, 16)

/*
** load_c10_uintcap_t_uint64_t_16:
**	ldr	c10, \[[cx]0, x1, lsl #?4\]
**	ret
*/
LOAD_REG_INDEX (c10, uintcap_t, uint64_t, 16)
