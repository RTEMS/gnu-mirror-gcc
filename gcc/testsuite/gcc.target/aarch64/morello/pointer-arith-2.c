/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-mfake-capability" } }  */

#include "pointer-arith-1.h"

/*
** test_int8_t_1:
**	add	x0, x0, w1, sxtb
**	ret
*/
TEST (int8_t, 1)

/*
** test_uint8_t_1:
**	add	x0, x0, w1, uxtb
**	ret
*/
TEST (uint8_t, 1)

/*
** test_int8_t_2:
**	add	x0, x0, w1, sxtb #?1
**	ret
*/
TEST (int8_t, 2)

/*
** test_uint8_t_2:
**	add	x0, x0, w1, uxtb #?1
**	ret
*/
TEST (uint8_t, 2)

/*
** test_int8_t_4:
**	add	x0, x0, w1, sxtb #?2
**	ret
*/
TEST (int8_t, 4)

/*
** test_uint8_t_4:
**	add	x0, x0, w1, uxtb #?2
**	ret
*/
TEST (uint8_t, 4)

/*
** test_int8_t_8:
**	add	x0, x0, w1, sxtb #?3
**	ret
*/
TEST (int8_t, 8)

/*
** test_uint8_t_8:
**	add	x0, x0, w1, uxtb #?3
**	ret
*/
TEST (uint8_t, 8)

/*
** test_int8_t_16:
**	add	x0, x0, w1, sxtb #?4
**	ret
*/
TEST (int8_t, 16)

/*
** test_uint8_t_16:
**	add	x0, x0, w1, uxtb #?4
**	ret
*/
TEST (uint8_t, 16)

/* Check for valid asm, but don't mandate a particular sequence.  */
TEST (int8_t, 32)
TEST (uint8_t, 32)

/*
** test_int16_t_1:
**	add	x0, x0, w1, sxth
**	ret
*/
TEST (int16_t, 1)

/*
** test_uint16_t_1:
**	add	x0, x0, w1, uxth
**	ret
*/
TEST (uint16_t, 1)

/*
** test_int16_t_2:
**	add	x0, x0, w1, sxth #?1
**	ret
*/
TEST (int16_t, 2)

/*
** test_uint16_t_2:
**	add	x0, x0, w1, uxth #?1
**	ret
*/
TEST (uint16_t, 2)

/*
** test_int16_t_4:
**	add	x0, x0, w1, sxth #?2
**	ret
*/
TEST (int16_t, 4)

/*
** test_uint16_t_4:
**	add	x0, x0, w1, uxth #?2
**	ret
*/
TEST (uint16_t, 4)

/*
** test_int16_t_8:
**	add	x0, x0, w1, sxth #?3
**	ret
*/
TEST (int16_t, 8)

/*
** test_uint16_t_8:
**	add	x0, x0, w1, uxth #?3
**	ret
*/
TEST (uint16_t, 8)

/*
** test_int16_t_16:
**	add	x0, x0, w1, sxth #?4
**	ret
*/
TEST (int16_t, 16)

/*
** test_uint16_t_16:
**	add	x0, x0, w1, uxth #?4
**	ret
*/
TEST (uint16_t, 16)

/* Check for valid asm, but don't mandate a particular sequence.  */
TEST (int16_t, 32)
TEST (uint16_t, 32)

/*
** test_int32_t_1:
**	add	x0, x0, w1, sxtw
**	ret
*/
TEST (int32_t, 1)

/*
** test_uint32_t_1:
**	add	x0, x0, w1, uxtw
**	ret
*/
TEST (uint32_t, 1)

/*
** test_int32_t_2:
**	add	x0, x0, w1, sxtw #?1
**	ret
*/
TEST (int32_t, 2)

/*
** test_uint32_t_2:
**	add	x0, x0, w1, uxtw #?1
**	ret
*/
TEST (uint32_t, 2)

/*
** test_int32_t_4:
**	add	x0, x0, w1, sxtw #?2
**	ret
*/
TEST (int32_t, 4)

/*
** test_uint32_t_4:
**	add	x0, x0, w1, uxtw #?2
**	ret
*/
TEST (uint32_t, 4)

/*
** test_int32_t_8:
**	add	x0, x0, w1, sxtw #?3
**	ret
*/
TEST (int32_t, 8)

/*
** test_uint32_t_8:
**	add	x0, x0, w1, uxtw #?3
**	ret
*/
TEST (uint32_t, 8)

/*
** test_int32_t_16:
**	add	x0, x0, w1, sxtw #?4
**	ret
*/
TEST (int32_t, 16)

/*
** test_uint32_t_16:
**	add	x0, x0, w1, uxtw #?4
**	ret
*/
TEST (uint32_t, 16)

/* Check for valid asm, but don't mandate a particular sequence.  */
TEST (int32_t, 32)
TEST (uint32_t, 32)

/*
** test_uint64_t_1:
**	add	x0, x0, x1
**	ret
*/
TEST (uint64_t, 1)

/*
** test_uint64_t_2:
**	add	x0, x0, x1, lsl #?1
**	ret
*/
TEST (uint64_t, 2)

/*
** test_uint64_t_4:
**	add	x0, x0, x1, lsl #?2
**	ret
*/
TEST (uint64_t, 4)

/*
** test_uint64_t_8:
**	add	x0, x0, x1, lsl #?3
**	ret
*/
TEST (uint64_t, 8)

/*
** test_uint64_t_16:
**	add	x0, x0, x1, lsl #?4
**	ret
*/
TEST (uint64_t, 16)

/* Check for valid asm, but don't mandate a particular sequence.  */
TEST (uint64_t, 32)
