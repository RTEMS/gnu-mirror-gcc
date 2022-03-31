/* { dg-do assemble } */
/* { dg-additional-options "-fno-ivopts -save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */

#include "load-store-utils.h"

/*
** pre_modify_double_m33:
**	...
**	sub	.*, #?264
**	...
*/
PRE_MODIFY_OFFSET (double, m33)

/*
** pre_modify_double_m32:
**	...
**	str	d[0-9]+, \[[xc][0-9]+, #?-256\]!
**	...
*/
PRE_MODIFY_OFFSET (double, m32)

/*
** pre_modify_double_m1:
**	...
**	str	d[0-9]+, \[[xc][0-9]+, #?-8\]!
**	...
*/
PRE_MODIFY_OFFSET (double, m1)

/*
** pre_modify_double_1:
**	...
**	str	d[0-9]+, \[[xc][0-9]+, #?8\]!
**	...
*/
PRE_MODIFY_OFFSET (double, 1)

/*
** pre_modify_double_31:
**	...
**	str	d[0-9]+, \[[xc][0-9]+, #?248\]!
**	...
*/
PRE_MODIFY_OFFSET (double, 31)

/*
** pre_modify_double_32:
**	...
**	add	.*, #?256
**	...
*/
PRE_MODIFY_OFFSET (double, 32)

/*
** post_modify_double_m33:
**	...
**	sub	.*, #?264
**	...
*/
POST_MODIFY_OFFSET (double, m33)

/*
** post_modify_double_m32:
**	...
**	str	d[0-9]+, \[[xc][0-9]+\], #?-256
**	...
*/
POST_MODIFY_OFFSET (double, m32)

/*
** post_modify_double_m1:
**	...
**	str	d[0-9]+, \[[xc][0-9]+\], #?-8
**	...
*/
POST_MODIFY_OFFSET (double, m1)

/*
** post_modify_double_1:
**	...
**	str	d[0-9]+, \[[xc][0-9]+\], #?8
**	...
*/
POST_MODIFY_OFFSET (double, 1)

/*
** post_modify_double_31:
**	...
**	str	d[0-9]+, \[[xc][0-9]+\], #?248
**	...
*/
POST_MODIFY_OFFSET (double, 31)

/*
** post_modify_double_32:
**	...
**	add	.*, #?256
**	...
*/
POST_MODIFY_OFFSET (double, 32)
