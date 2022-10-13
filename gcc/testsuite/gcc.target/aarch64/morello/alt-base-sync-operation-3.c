/* { dg-do assemble } */
/* { dg-additional-options "-foptimize-sibling-calls -save-temps -Wno-cheri-implicit-pointer-conversion-from-cap -Wno-cheri-explicit-pointer-conversion-from-cap" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <stdint.h>

typedef __uint128_t uint128;
typedef __intcap intcap;

#define TEST_OPERATION(TYPE, SIZE, OPERATION)				\
  TYPE									\
  test_##TYPE##_fetch_and_##OPERATION (TYPE *__capability ptr, TYPE val) \
  {									\
    return __sync_fetch_and_##OPERATION (ptr, val);			\
  }									\
									\
  TYPE									\
  test_##TYPE##_##OPERATION##_and_fetch (TYPE *__capability ptr, TYPE val) \
  {									\
    return __sync_##OPERATION##_and_fetch (ptr, val);			\
  }

#define TEST_SIZE(TYPE, SIZE)				\
  TEST_OPERATION (TYPE, SIZE, add)			\
  TEST_OPERATION (TYPE, SIZE, sub)			\
  TEST_OPERATION (TYPE, SIZE, and)			\
  TEST_OPERATION (TYPE, SIZE, nand)			\
  TEST_OPERATION (TYPE, SIZE, or)			\
  TEST_OPERATION (TYPE, SIZE, xor)

/*
** test_uint8_t_fetch_and_add:
**	b	__sync_fetch_and_add_1_c
*/

/*
** test_uint8_t_fetch_and_sub:
**	b	__sync_fetch_and_sub_1_c
*/

/*
** test_uint8_t_fetch_and_and:
**	b	__sync_fetch_and_and_1_c
*/

/*
** test_uint8_t_fetch_and_nand:
**	b	__sync_fetch_and_nand_1_c
*/

/*
** test_uint8_t_fetch_and_or:
**	b	__sync_fetch_and_or_1_c
*/

/*
** test_uint8_t_fetch_and_xor:
**	b	__sync_fetch_and_xor_1_c
*/

/*
** test_uint8_t_add_and_fetch:
**	b	__sync_add_and_fetch_1_c
*/

/*
** test_uint8_t_sub_and_fetch:
**	b	__sync_sub_and_fetch_1_c
*/

/*
** test_uint8_t_and_and_fetch:
**	b	__sync_and_and_fetch_1_c
*/

/*
** test_uint8_t_nand_and_fetch:
**	b	__sync_nand_and_fetch_1_c
*/

/*
** test_uint8_t_or_and_fetch:
**	b	__sync_or_and_fetch_1_c
*/

/*
** test_uint8_t_xor_and_fetch:
**	b	__sync_xor_and_fetch_1_c
*/
TEST_SIZE (uint8_t, 1)

/*
** test_uint16_t_fetch_and_add:
**	b	__sync_fetch_and_add_2_c
*/

/*
** test_uint16_t_fetch_and_sub:
**	b	__sync_fetch_and_sub_2_c
*/

/*
** test_uint16_t_fetch_and_and:
**	b	__sync_fetch_and_and_2_c
*/

/*
** test_uint16_t_fetch_and_nand:
**	b	__sync_fetch_and_nand_2_c
*/

/*
** test_uint16_t_fetch_and_or:
**	b	__sync_fetch_and_or_2_c
*/

/*
** test_uint16_t_fetch_and_xor:
**	b	__sync_fetch_and_xor_2_c
*/

/*
** test_uint16_t_add_and_fetch:
**	b	__sync_add_and_fetch_2_c
*/

/*
** test_uint16_t_sub_and_fetch:
**	b	__sync_sub_and_fetch_2_c
*/

/*
** test_uint16_t_and_and_fetch:
**	b	__sync_and_and_fetch_2_c
*/

/*
** test_uint16_t_nand_and_fetch:
**	b	__sync_nand_and_fetch_2_c
*/

/*
** test_uint16_t_or_and_fetch:
**	b	__sync_or_and_fetch_2_c
*/

/*
** test_uint16_t_xor_and_fetch:
**	b	__sync_xor_and_fetch_2_c
*/
TEST_SIZE (uint16_t, 2)

/*
** test_uint32_t_fetch_and_add:
**	b	__sync_fetch_and_add_4_c
*/

/*
** test_uint32_t_fetch_and_sub:
**	b	__sync_fetch_and_sub_4_c
*/

/*
** test_uint32_t_fetch_and_and:
**	b	__sync_fetch_and_and_4_c
*/

/*
** test_uint32_t_fetch_and_nand:
**	b	__sync_fetch_and_nand_4_c
*/

/*
** test_uint32_t_fetch_and_or:
**	b	__sync_fetch_and_or_4_c
*/

/*
** test_uint32_t_fetch_and_xor:
**	b	__sync_fetch_and_xor_4_c
*/

/*
** test_uint32_t_add_and_fetch:
**	b	__sync_add_and_fetch_4_c
*/

/*
** test_uint32_t_sub_and_fetch:
**	b	__sync_sub_and_fetch_4_c
*/

/*
** test_uint32_t_and_and_fetch:
**	b	__sync_and_and_fetch_4_c
*/

/*
** test_uint32_t_nand_and_fetch:
**	b	__sync_nand_and_fetch_4_c
*/

/*
** test_uint32_t_or_and_fetch:
**	b	__sync_or_and_fetch_4_c
*/

/*
** test_uint32_t_xor_and_fetch:
**	b	__sync_xor_and_fetch_4_c
*/
TEST_SIZE (uint32_t, 4)

/*
** test_uint64_t_fetch_and_add:
**	b	__sync_fetch_and_add_8_c
*/

/*
** test_uint64_t_fetch_and_sub:
**	b	__sync_fetch_and_sub_8_c
*/

/*
** test_uint64_t_fetch_and_and:
**	b	__sync_fetch_and_and_8_c
*/

/*
** test_uint64_t_fetch_and_nand:
**	b	__sync_fetch_and_nand_8_c
*/

/*
** test_uint64_t_fetch_and_or:
**	b	__sync_fetch_and_or_8_c
*/

/*
** test_uint64_t_fetch_and_xor:
**	b	__sync_fetch_and_xor_8_c
*/

/*
** test_uint64_t_add_and_fetch:
**	b	__sync_add_and_fetch_8_c
*/

/*
** test_uint64_t_sub_and_fetch:
**	b	__sync_sub_and_fetch_8_c
*/

/*
** test_uint64_t_and_and_fetch:
**	b	__sync_and_and_fetch_8_c
*/

/*
** test_uint64_t_nand_and_fetch:
**	b	__sync_nand_and_fetch_8_c
*/

/*
** test_uint64_t_or_and_fetch:
**	b	__sync_or_and_fetch_8_c
*/

/*
** test_uint64_t_xor_and_fetch:
**	b	__sync_xor_and_fetch_8_c
*/
TEST_SIZE (uint64_t, 8)

/*
** test_uint128_fetch_and_add:
**	b	__sync_fetch_and_add_16_c
*/

/*
** test_uint128_fetch_and_sub:
**	b	__sync_fetch_and_sub_16_c
*/

/*
** test_uint128_fetch_and_and:
**	b	__sync_fetch_and_and_16_c
*/

/*
** test_uint128_fetch_and_nand:
**	b	__sync_fetch_and_nand_16_c
*/

/*
** test_uint128_fetch_and_or:
**	b	__sync_fetch_and_or_16_c
*/

/*
** test_uint128_fetch_and_xor:
**	b	__sync_fetch_and_xor_16_c
*/

/*
** test_uint128_add_and_fetch:
**	b	__sync_add_and_fetch_16_c
*/

/*
** test_uint128_sub_and_fetch:
**	b	__sync_sub_and_fetch_16_c
*/

/*
** test_uint128_and_and_fetch:
**	b	__sync_and_and_fetch_16_c
*/

/*
** test_uint128_nand_and_fetch:
**	b	__sync_nand_and_fetch_16_c
*/

/*
** test_uint128_or_and_fetch:
**	b	__sync_or_and_fetch_16_c
*/

/*
** test_uint128_xor_and_fetch:
**	b	__sync_xor_and_fetch_16_c
*/
TEST_SIZE (uint128, 16)

/*
** test_intcap_fetch_and_add:
**	b	__sync_fetch_and_add_capability_c
*/

/*
** test_intcap_fetch_and_sub:
**	b	__sync_fetch_and_sub_capability_c
*/

/*
** test_intcap_fetch_and_and:
**	b	__sync_fetch_and_and_capability_c
*/

/*
** test_intcap_fetch_and_nand:
**	b	__sync_fetch_and_nand_capability_c
*/

/*
** test_intcap_fetch_and_or:
**	b	__sync_fetch_and_or_capability_c
*/

/*
** test_intcap_fetch_and_xor:
**	b	__sync_fetch_and_xor_capability_c
*/

/*
** test_intcap_add_and_fetch:
**	b	__sync_add_and_fetch_capability_c
*/

/*
** test_intcap_sub_and_fetch:
**	b	__sync_sub_and_fetch_capability_c
*/

/*
** test_intcap_and_and_fetch:
**	b	__sync_and_and_fetch_capability_c
*/

/*
** test_intcap_nand_and_fetch:
**	b	__sync_nand_and_fetch_capability_c
*/

/*
** test_intcap_or_and_fetch:
**	b	__sync_or_and_fetch_capability_c
*/

/*
** test_intcap_xor_and_fetch:
**	b	__sync_xor_and_fetch_capability_c
*/
TEST_SIZE (intcap, capability)
