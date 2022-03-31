/* { dg-do assemble } */
/* { dg-additional-options "-fno-ivopts -save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */

#include "load-store-utils.h"

/*
** pre_modify_uint64_t_m33:
**	...
**	sub	.*, #?264
**	...
*/
PRE_MODIFY_OFFSET (uint64_t, m33)

/*
** pre_modify_uint64_t_m32:
**	...
**	str	x[0-9]+, \[[xc][0-9]+, #?-256\]!
**	...
*/
PRE_MODIFY_OFFSET (uint64_t, m32)

/*
** pre_modify_uint64_t_m1:
**	...
**	str	x[0-9]+, \[[xc][0-9]+, #?-8\]!
**	...
*/
PRE_MODIFY_OFFSET (uint64_t, m1)

/*
** pre_modify_uint64_t_1:
**	...
**	str	x[0-9]+, \[[xc][0-9]+, #?8\]!
**	...
*/
PRE_MODIFY_OFFSET (uint64_t, 1)

/*
** pre_modify_uint64_t_31:
**	...
**	str	x[0-9]+, \[[xc][0-9]+, #?248\]!
**	...
*/
PRE_MODIFY_OFFSET (uint64_t, 31)

/*
** pre_modify_uint64_t_32:
**	...
**	add	.*, #?256
**	...
*/
PRE_MODIFY_OFFSET (uint64_t, 32)

/*
** post_modify_uint64_t_m33:
**	...
**	sub	.*, #?264
**	...
*/
POST_MODIFY_OFFSET (uint64_t, m33)

/*
** post_modify_uint64_t_m32:
**	...
**	str	x[0-9]+, \[[xc][0-9]+\], #?-256
**	...
*/
POST_MODIFY_OFFSET (uint64_t, m32)

/*
** post_modify_uint64_t_m1:
**	...
**	str	x[0-9]+, \[[xc][0-9]+\], #?-8
**	...
*/
POST_MODIFY_OFFSET (uint64_t, m1)

/*
** post_modify_uint64_t_1:
**	...
**	str	x[0-9]+, \[[xc][0-9]+\], #?8
**	...
*/
POST_MODIFY_OFFSET (uint64_t, 1)

/*
** post_modify_uint64_t_31:
**	...
**	str	x[0-9]+, \[[xc][0-9]+\], #?248
**	...
*/
POST_MODIFY_OFFSET (uint64_t, 31)

/*
** post_modify_uint64_t_32:
**	...
**	add	.*, #?256
**	...
*/
POST_MODIFY_OFFSET (uint64_t, 32)
