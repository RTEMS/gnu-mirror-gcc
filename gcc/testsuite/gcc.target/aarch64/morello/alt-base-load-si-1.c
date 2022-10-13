/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

/*
** load_w10_uint32_t_m260:
**	sub	(c[0-9]+), c0, #260
**	ldr	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, m260)

/*
** load_w10_uint32_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, m257)

/*
** load_w10_uint32_t_m256:
**	ldr	w10, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, m256)

/*
** load_w10_uint32_t_m252:
**	ldr	w10, \[c0, #?-252\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, m252)

/*
** load_w10_uint32_t_m4:
**	ldr	w10, \[c0, #?-4\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, m4)

/*
** load_w10_uint32_t_m1:
**	ldr	w10, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, m1)

/*
** load_w10_uint32_t_1:
**	ldr	w10, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, 1)

/*
** load_w10_uint32_t_4:
**	ldr	w10, \[c0, #?4\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, 4)

/*
** load_w10_uint32_t_252:
**	ldr	w10, \[c0, #?252\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, 252)

/*
** load_w10_uint32_t_255:
**	ldr	w10, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, 255)

/*
** load_w10_uint32_t_256:
**	ldr	w10, \[c0, #?256\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, 256)

/*
** load_w10_uint32_t_257:
**	add	(c[0-9]+), c0, #?256
**	ldr	w10, \[\1, #?1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, 257)

/*
** load_w10_uint32_t_260:
**	ldr	w10, \[c0, #?260\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, 260)

/*
** load_w10_uint32_t_2044:
**	ldr	w10, \[c0, #?2044\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, 2044)

/*
** load_w10_uint32_t_2048:
**	add	(c[0-9]+), c0, #?2048
**	ldr	w10, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (w10, uint32_t, 2048)

/*
** load_w10_uint32_t_int32_t_1:
**	ldr	w10, \[c0, w1, sxtw\]
**	ret
*/
LOAD_REG_INDEX (w10, uint32_t, int32_t, 1)

/*
** load_w10_uint32_t_uint32_t_1:
**	ldr	w10, \[c0, w1, uxtw\]
**	ret
*/
LOAD_REG_INDEX (w10, uint32_t, uint32_t, 1)

/*
** load_w10_uint32_t_uint64_t_1:
**	ldr	w10, \[c0, x1\]
**	ret
*/
LOAD_REG_INDEX (w10, uint32_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (w10, uint32_t, int32_t, 2)
LOAD_REG_INDEX (w10, uint32_t, uint32_t, 2)
LOAD_REG_INDEX (w10, uint32_t, uint64_t, 2)

/*
** load_w10_uint32_t_int32_t_4:
**	ldr	w10, \[c0, w1, sxtw #?2\]
**	ret
*/
LOAD_REG_INDEX (w10, uint32_t, int32_t, 4)

/*
** load_w10_uint32_t_uint32_t_4:
**	ldr	w10, \[c0, w1, uxtw #?2\]
**	ret
*/
LOAD_REG_INDEX (w10, uint32_t, uint32_t, 4)

/*
** load_w10_uint32_t_uint64_t_4:
**	ldr	w10, \[c0, x1, lsl #?2\]
**	ret
*/
LOAD_REG_INDEX (w10, uint32_t, uint64_t, 4)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (w10, uint32_t, int32_t, 8)
LOAD_REG_INDEX (w10, uint32_t, uint32_t, 8)
LOAD_REG_INDEX (w10, uint32_t, uint64_t, 8)

LOAD_REG_INDEX (w10, uint32_t, int32_t, 16)
LOAD_REG_INDEX (w10, uint32_t, uint32_t, 16)
LOAD_REG_INDEX (w10, uint32_t, uint64_t, 16)

/*
** load_s20_uint32_t_m260:
**	sub	(c[0-9]+), c0, #260
**	ldr	s20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (s20, uint32_t, m260)

/*
** load_s20_uint32_t_m257:
**	sub	(c[0-9]+), c0, #257
**	ldr	s20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (s20, uint32_t, m257)

/*
** load_s20_uint32_t_m256:
**	ldr	s20, \[c0, #?-256\]
**	ret
*/
LOAD_REG_OFFSET (s20, uint32_t, m256)

/*
** load_s20_uint32_t_m252:
**	ldr	s20, \[c0, #?-252\]
**	ret
*/
LOAD_REG_OFFSET (s20, uint32_t, m252)

/*
** load_s20_uint32_t_m4:
**	ldr	s20, \[c0, #?-4\]
**	ret
*/
LOAD_REG_OFFSET (s20, uint32_t, m4)

/*
** load_s20_uint32_t_m1:
**	ldr	s20, \[c0, #?-1\]
**	ret
*/
LOAD_REG_OFFSET (s20, uint32_t, m1)

/*
** load_s20_uint32_t_1:
**	ldr	s20, \[c0, #?1\]
**	ret
*/
LOAD_REG_OFFSET (s20, uint32_t, 1)

/*
** load_s20_uint32_t_4:
**	ldr	s20, \[c0, #?4\]
**	ret
*/
LOAD_REG_OFFSET (s20, uint32_t, 4)

/*
** load_s20_uint32_t_252:
**	ldr	s20, \[c0, #?252\]
**	ret
*/
LOAD_REG_OFFSET (s20, uint32_t, 252)

/*
** load_s20_uint32_t_255:
**	ldr	s20, \[c0, #?255\]
**	ret
*/
LOAD_REG_OFFSET (s20, uint32_t, 255)

/*
** load_s20_uint32_t_256:
**	add	(c[0-9]+), c0, #?256
**	ldr	s20, \[\1\]
**	ret
*/
LOAD_REG_OFFSET (s20, uint32_t, 256)

/*
** load_s20_uint32_t_int32_t_1:
**	add	(c[0-9]+), c0, w1, sxtw
**	ldr	s20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (s20, uint32_t, int32_t, 1)

/*
** load_s20_uint32_t_uint32_t_1:
**	add	(c[0-9]+), c0, w1, uxtw
**	ldr	s20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (s20, uint32_t, uint32_t, 1)

/*
** load_s20_uint32_t_uint64_t_1:
**	add	(c[0-9]+), c0, x1
**	ldr	s20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (s20, uint32_t, uint64_t, 1)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (s20, uint32_t, int32_t, 2)
LOAD_REG_INDEX (s20, uint32_t, uint32_t, 2)
LOAD_REG_INDEX (s20, uint32_t, uint64_t, 2)

/*
** load_s20_uint32_t_int32_t_4:
**	add	(c[0-9]+), c0, w1, sxtw #?2
**	ldr	s20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (s20, uint32_t, int32_t, 4)

/*
** load_s20_uint32_t_uint32_t_4:
**	add	(c[0-9]+), c0, w1, uxtw #?2
**	ldr	s20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (s20, uint32_t, uint32_t, 4)

/*
** load_s20_uint32_t_uint64_t_4:
**	add	(c[0-9]+), c0, x1, lsl #?2
**	ldr	s20, \[\1\]
**	ret
*/
LOAD_REG_INDEX (s20, uint32_t, uint64_t, 4)

/* Check for valid asm, but don't mandate a particular sequence.  */
LOAD_REG_INDEX (s20, uint32_t, int32_t, 8)
LOAD_REG_INDEX (s20, uint32_t, uint32_t, 8)
LOAD_REG_INDEX (s20, uint32_t, uint64_t, 8)

LOAD_REG_INDEX (s20, uint32_t, int32_t, 16)
LOAD_REG_INDEX (s20, uint32_t, uint32_t, 16)
LOAD_REG_INDEX (s20, uint32_t, uint64_t, 16)
