/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-skip-if "" { *-*-* } { "-mfake-capability" } { "" } }  */

#include "load-store-utils.h"

typedef __uintcap_t uintcap_t;

/* Must assemble correctly, but don't require a specific sequence.  */
PRE_MODIFY_OFFSET (uintcap_t, m257)

/*
** pre_modify_uintcap_t_m256:
**	...
**	str	c[0-9]+, \[[xc][0-9]+, #?-4096\]!
**	...
*/
PRE_MODIFY_OFFSET (uintcap_t, m256)

/*
** pre_modify_uintcap_t_m1:
**	...
**	str	c[0-9]+, \[[xc][0-9]+, #?-16\]!
**	...
*/
PRE_MODIFY_OFFSET (uintcap_t, m1)

/*
** pre_modify_uintcap_t_1:
**	...
**	str	c[0-9]+, \[[xc][0-9]+, #?16\]!
**	...
*/
PRE_MODIFY_OFFSET (uintcap_t, 1)

/*
** pre_modify_uintcap_t_255:
**	...
**	str	c[0-9]+, \[[xc][0-9]+, #?4080\]!
**	...
*/
PRE_MODIFY_OFFSET (uintcap_t, 255)

/* Must assemble correctly, but don't require a specific sequence.  */
PRE_MODIFY_OFFSET (uintcap_t, 16)

/* Must assemble correctly, but don't require a specific sequence.  */
POST_MODIFY_OFFSET (uintcap_t, m257)

/*
** post_modify_uintcap_t_m256:
**	...
**	str	c[0-9]+, \[[xc][0-9]+\], #?-4096
**	...
*/
POST_MODIFY_OFFSET (uintcap_t, m256)

/*
** post_modify_uintcap_t_m1:
**	...
**	str	c[0-9]+, \[[xc][0-9]+\], #?-16
**	...
*/
POST_MODIFY_OFFSET (uintcap_t, m1)

/*
** post_modify_uintcap_t_1:
**	...
**	str	c[0-9]+, \[[xc][0-9]+\], #?16
**	...
*/
POST_MODIFY_OFFSET (uintcap_t, 1)

/*
** post_modify_uintcap_t_255:
**	...
**	str	c[0-9]+, \[[xc][0-9]+\], #?4080
**	...
*/
POST_MODIFY_OFFSET (uintcap_t, 255)

/* Must assemble correctly, but don't require a specific sequence.  */
POST_MODIFY_OFFSET (uintcap_t, 256)
