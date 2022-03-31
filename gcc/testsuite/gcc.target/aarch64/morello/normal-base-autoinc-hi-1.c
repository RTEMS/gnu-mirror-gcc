/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */

#include "load-store-utils.h"

/*
** pre_modify_uint16_t_m129:
**	...
**	sub	.*, #?258
**	...
*/
PRE_MODIFY_OFFSET (uint16_t, m129)

/*
** pre_modify_uint16_t_m128:
**	...
**	strh	w[0-9]+, \[[xc][0-9]+, #?-256\]!
**	...
*/
PRE_MODIFY_OFFSET (uint16_t, m128)

/*
** pre_modify_uint16_t_m1:
**	...
**	strh	w[0-9]+, \[[xc][0-9]+, #?-2\]!
**	...
*/
PRE_MODIFY_OFFSET (uint16_t, m1)

/*
** pre_modify_uint16_t_1:
**	...
**	strh	w[0-9]+, \[[xc][0-9]+, #?2\]!
**	...
*/
PRE_MODIFY_OFFSET (uint16_t, 1)

/*
** pre_modify_uint16_t_127:
**	...
**	strh	w[0-9]+, \[[xc][0-9]+, #?254\]!
**	...
*/
PRE_MODIFY_OFFSET (uint16_t, 127)

/*
** pre_modify_uint16_t_128:
**	...
**	add	.*, #?256
**	...
*/
PRE_MODIFY_OFFSET (uint16_t, 128)

/*
** post_modify_uint16_t_m129:
**	...
**	sub	.*, #?258
**	...
*/
POST_MODIFY_OFFSET (uint16_t, m129)

/*
** post_modify_uint16_t_m128:
**	...
**	strh	w[0-9]+, \[[xc][0-9]+\], #?-256
**	...
*/
POST_MODIFY_OFFSET (uint16_t, m128)

/*
** post_modify_uint16_t_m1:
**	...
**	strh	w[0-9]+, \[[xc][0-9]+\], #?-2
**	...
*/
POST_MODIFY_OFFSET (uint16_t, m1)

/*
** post_modify_uint16_t_1:
**	...
**	strh	w[0-9]+, \[[xc][0-9]+\], #?2
**	...
*/
POST_MODIFY_OFFSET (uint16_t, 1)

/*
** post_modify_uint16_t_127:
**	...
**	strh	w[0-9]+, \[[xc][0-9]+\], #?254
**	...
*/
POST_MODIFY_OFFSET (uint16_t, 127)

/*
** post_modify_uint16_t_128:
**	...
**	add	.*, #?256
**	...
*/
POST_MODIFY_OFFSET (uint16_t, 128)
