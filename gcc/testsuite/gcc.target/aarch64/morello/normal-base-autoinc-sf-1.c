/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */

#include "load-store-utils.h"

/*
** pre_modify_float_m65:
**	...
**	sub	.*, #?260
**	...
*/
PRE_MODIFY_OFFSET (float, m65)

/*
** pre_modify_float_m64:
**	...
**	str	s[0-9]+, \[[xc][0-9]+, #?-256\]!
**	...
*/
PRE_MODIFY_OFFSET (float, m64)

/*
** pre_modify_float_m1:
**	...
**	str	s[0-9]+, \[[xc][0-9]+, #?-4\]!
**	...
*/
PRE_MODIFY_OFFSET (float, m1)

/*
** pre_modify_float_1:
**	...
**	str	s[0-9]+, \[[xc][0-9]+, #?4\]!
**	...
*/
PRE_MODIFY_OFFSET (float, 1)

/*
** pre_modify_float_63:
**	...
**	str	s[0-9]+, \[[xc][0-9]+, #?252\]!
**	...
*/
PRE_MODIFY_OFFSET (float, 63)

/*
** pre_modify_float_64:
**	...
**	add	.*, #?256
**	...
*/
PRE_MODIFY_OFFSET (float, 64)

/*
** post_modify_float_m65:
**	...
**	sub	.*, #?260
**	...
*/
POST_MODIFY_OFFSET (float, m65)

/*
** post_modify_float_m64:
**	...
**	str	s[0-9]+, \[[xc][0-9]+\], #?-256
**	...
*/
POST_MODIFY_OFFSET (float, m64)

/*
** post_modify_float_m1:
**	...
**	str	s[0-9]+, \[[xc][0-9]+\], #?-4
**	...
*/
POST_MODIFY_OFFSET (float, m1)

/*
** post_modify_float_1:
**	...
**	str	s[0-9]+, \[[xc][0-9]+\], #?4
**	...
*/
POST_MODIFY_OFFSET (float, 1)

/*
** post_modify_float_63:
**	...
**	str	s[0-9]+, \[[xc][0-9]+\], #?252
**	...
*/
POST_MODIFY_OFFSET (float, 63)

/*
** post_modify_float_64:
**	...
**	add	.*, #?256
**	...
*/
POST_MODIFY_OFFSET (float, 64)
