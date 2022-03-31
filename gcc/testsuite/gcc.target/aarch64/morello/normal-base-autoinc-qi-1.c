/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */

#include "load-store-utils.h"

/*
** pre_modify_uint8_t_m257:
**	...
**	sub	.*, #?257
**	...
*/
PRE_MODIFY_OFFSET (uint8_t, m257)

/*
** pre_modify_uint8_t_m256:
**	...
**	strb	w[0-9]+, \[[xc][0-9]+, #?-256\]!
**	...
*/
PRE_MODIFY_OFFSET (uint8_t, m256)

/*
** pre_modify_uint8_t_m1:
**	...
**	strb	w[0-9]+, \[[xc][0-9]+, #?-1\]!
**	...
*/
PRE_MODIFY_OFFSET (uint8_t, m1)

/*
** pre_modify_uint8_t_1:
**	...
**	strb	w[0-9]+, \[[xc][0-9]+, #?1\]!
**	...
*/
PRE_MODIFY_OFFSET (uint8_t, 1)

/*
** pre_modify_uint8_t_255:
**	...
**	strb	w[0-9]+, \[[xc][0-9]+, #?255\]!
**	...
*/
PRE_MODIFY_OFFSET (uint8_t, 255)

/*
** pre_modify_uint8_t_256:
**	...
**	add	.*, #?256
**	...
*/
PRE_MODIFY_OFFSET (uint8_t, 256)

/*
** post_modify_uint8_t_m257:
**	...
**	sub	.*, #?257
**	...
*/
POST_MODIFY_OFFSET (uint8_t, m257)

/*
** post_modify_uint8_t_m256:
**	...
**	strb	w[0-9]+, \[[xc][0-9]+\], #?-256
**	...
*/
POST_MODIFY_OFFSET (uint8_t, m256)

/*
** post_modify_uint8_t_m1:
**	...
**	strb	w[0-9]+, \[[xc][0-9]+\], #?-1
**	...
*/
POST_MODIFY_OFFSET (uint8_t, m1)

/*
** post_modify_uint8_t_1:
**	...
**	strb	w[0-9]+, \[[xc][0-9]+\], #?1
**	...
*/
POST_MODIFY_OFFSET (uint8_t, 1)

/*
** post_modify_uint8_t_255:
**	...
**	strb	w[0-9]+, \[[xc][0-9]+\], #?255
**	...
*/
POST_MODIFY_OFFSET (uint8_t, 255)

/*
** post_modify_uint8_t_256:
**	...
**	add	.*, #?256
**	...
*/
POST_MODIFY_OFFSET (uint8_t, 256)
