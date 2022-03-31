/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */

#include "load-store-utils.h"

/*
** pre_modify_uint32_t_m65:
**	...
**	sub	.*, #?260
**	...
*/
PRE_MODIFY_OFFSET (uint32_t, m65)

/*
** pre_modify_uint32_t_m64:
**	...
**	str	w[0-9]+, \[[xc][0-9]+, #?-256\]!
**	...
*/
PRE_MODIFY_OFFSET (uint32_t, m64)

/*
** pre_modify_uint32_t_m1:
**	...
**	str	w[0-9]+, \[[xc][0-9]+, #?-4\]!
**	...
*/
PRE_MODIFY_OFFSET (uint32_t, m1)

/*
** pre_modify_uint32_t_1:
**	...
**	str	w[0-9]+, \[[xc][0-9]+, #?4\]!
**	...
*/
PRE_MODIFY_OFFSET (uint32_t, 1)

/*
** pre_modify_uint32_t_63:
**	...
**	str	w[0-9]+, \[[xc][0-9]+, #?252\]!
**	...
*/
PRE_MODIFY_OFFSET (uint32_t, 63)

/*
** pre_modify_uint32_t_64:
**	...
**	add	.*, #?256
**	...
*/
PRE_MODIFY_OFFSET (uint32_t, 64)

/*
** post_modify_uint32_t_m65:
**	...
**	sub	.*, #?260
**	...
*/
POST_MODIFY_OFFSET (uint32_t, m65)

/*
** post_modify_uint32_t_m64:
**	...
**	str	w[0-9]+, \[[xc][0-9]+\], #?-256
**	...
*/
POST_MODIFY_OFFSET (uint32_t, m64)

/*
** post_modify_uint32_t_m1:
**	...
**	str	w[0-9]+, \[[xc][0-9]+\], #?-4
**	...
*/
POST_MODIFY_OFFSET (uint32_t, m1)

/*
** post_modify_uint32_t_1:
**	...
**	str	w[0-9]+, \[[xc][0-9]+\], #?4
**	...
*/
POST_MODIFY_OFFSET (uint32_t, 1)

/*
** post_modify_uint32_t_63:
**	...
**	str	w[0-9]+, \[[xc][0-9]+\], #?252
**	...
*/
POST_MODIFY_OFFSET (uint32_t, 63)

/*
** post_modify_uint32_t_64:
**	...
**	add	.*, #?256
**	...
*/
POST_MODIFY_OFFSET (uint32_t, 64)
