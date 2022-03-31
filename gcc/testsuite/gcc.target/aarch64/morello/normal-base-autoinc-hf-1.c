/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */

#include "load-store-utils.h"

typedef __fp16 fp16;

/*
** pre_modify_fp16_m129:
**	...
**	sub	.*, #?258
**	...
*/
PRE_MODIFY_OFFSET (fp16, m129)

/*
** pre_modify_fp16_m128:
**	...
**	str	h[0-9]+, \[[xc][0-9]+, #?-256\]!
**	...
*/
PRE_MODIFY_OFFSET (fp16, m128)

/*
** pre_modify_fp16_m1:
**	...
**	str	h[0-9]+, \[[xc][0-9]+, #?-2\]!
**	...
*/
PRE_MODIFY_OFFSET (fp16, m1)

/*
** pre_modify_fp16_1:
**	...
**	str	h[0-9]+, \[[xc][0-9]+, #?2\]!
**	...
*/
PRE_MODIFY_OFFSET (fp16, 1)

/*
** pre_modify_fp16_127:
**	...
**	str	h[0-9]+, \[[xc][0-9]+, #?254\]!
**	...
*/
PRE_MODIFY_OFFSET (fp16, 127)

/*
** pre_modify_fp16_128:
**	...
**	add	.*, #?256
**	...
*/
PRE_MODIFY_OFFSET (fp16, 128)

/*
** post_modify_fp16_m129:
**	...
**	sub	.*, #?258
**	...
*/
POST_MODIFY_OFFSET (fp16, m129)

/*
** post_modify_fp16_m128:
**	...
**	str	h[0-9]+, \[[xc][0-9]+\], #?-256
**	...
*/
POST_MODIFY_OFFSET (fp16, m128)

/*
** post_modify_fp16_m1:
**	...
**	str	h[0-9]+, \[[xc][0-9]+\], #?-2
**	...
*/
POST_MODIFY_OFFSET (fp16, m1)

/*
** post_modify_fp16_1:
**	...
**	str	h[0-9]+, \[[xc][0-9]+\], #?2
**	...
*/
POST_MODIFY_OFFSET (fp16, 1)

/*
** post_modify_fp16_127:
**	...
**	str	h[0-9]+, \[[xc][0-9]+\], #?254
**	...
*/
POST_MODIFY_OFFSET (fp16, 127)

/*
** post_modify_fp16_128:
**	...
**	add	.*, #?256
**	...
*/
POST_MODIFY_OFFSET (fp16, 128)
