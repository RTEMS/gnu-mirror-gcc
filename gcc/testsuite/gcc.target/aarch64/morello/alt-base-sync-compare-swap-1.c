/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_SIZE(TYPE, SIZE)						\
  _Bool									\
  bool_##TYPE (TYPE *__capability ptr, TYPE oldval, TYPE newval) 	\
  {									\
    return __sync_bool_compare_and_swap_##SIZE##_c (ptr, oldval,	\
						    newval);		\
  }									\
									\
  TYPE									\
  val_##TYPE (TYPE *__capability ptr, TYPE oldval, TYPE newval)		\
  {									\
    return __sync_val_compare_and_swap_##SIZE##_c (ptr, oldval,		\
						   newval);		\
  }

/*
** bool_uint8_t:
**	b	__sync_bool_compare_and_swap_1_c
*/

/*
** val_uint8_t:
**	b	__sync_val_compare_and_swap_1_c
*/
TEST_SIZE (uint8_t, 1)

/*
** bool_uint16_t:
**	b	__sync_bool_compare_and_swap_2_c
*/

/*
** val_uint16_t:
**	b	__sync_val_compare_and_swap_2_c
*/
TEST_SIZE (uint16_t, 2)

/*
** bool_uint32_t:
**	b	__sync_bool_compare_and_swap_4_c
*/

/*
** val_uint32_t:
**	b	__sync_val_compare_and_swap_4_c
*/
TEST_SIZE (uint32_t, 4)

/*
** bool_uint64_t:
**	b	__sync_bool_compare_and_swap_8_c
*/

/*
** val_uint64_t:
**	b	__sync_val_compare_and_swap_8_c
*/
TEST_SIZE (uint64_t, 8)

/*
** bool_uint128:
**	b	__sync_bool_compare_and_swap_16_c
*/

/*
** val_uint128:
**	b	__sync_val_compare_and_swap_16_c
*/
TEST_SIZE (uint128, 16)

/*
** bool_intcap:
**	b	__sync_bool_compare_and_swap_capability_c
*/

/*
** val_intcap:
**	b	__sync_val_compare_and_swap_capability_c
*/
TEST_SIZE (intcap, capability)
