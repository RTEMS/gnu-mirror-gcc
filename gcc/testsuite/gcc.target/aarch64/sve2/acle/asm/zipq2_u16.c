/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** zipq2_u16_tied1:
**	zipq2	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (zipq2_u16_tied1, svuint16_t,
		z0 = svzipq2_u16 (z0, z1),
		z0 = svzipq2 (z0, z1))

/*
** zipq2_u16_tied2:
**	zipq2	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (zipq2_u16_tied2, svuint16_t,
		z0 = svzipq2_u16 (z1, z0),
		z0 = svzipq2 (z1, z0))

/*
** zipq2_u16_untied:
**	zipq2	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (zipq2_u16_untied, svuint16_t,
		z0 = svzipq2_u16 (z1, z2),
		z0 = svzipq2 (z1, z2))
