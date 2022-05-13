/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=armv8.2-a -std=c11" { target { ! cheri_capability_pure } } } */
/* { dg-additional-options "-std=c11" { target { cheri_capability_pure } } } */

#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>
void atomic_check_valid_1(int **intptrptr, int *intptr, int intval,
			  _Bool tempbool, _Bool* tempboolptr, size_t size,
			  void* voidptr) {
  intptr = __sync_fetch_and_add(intptrptr, intval);
  intptr = __sync_fetch_and_sub(intptrptr, intval);
  intptr = __sync_add_and_fetch(intptrptr, intval);
  intptr = __sync_sub_and_fetch(intptrptr, intval);
  intptr = __sync_fetch_and_and(intptrptr, intval);
  intptr = __sync_fetch_and_or(intptrptr, intval);
  intptr = __sync_fetch_and_xor(intptrptr, intval);
  intptr = __sync_fetch_and_nand(intptrptr, intval);
  intptr = __sync_and_and_fetch(intptrptr, intval);
  intptr = __sync_or_and_fetch(intptrptr, intval);
  intptr = __sync_xor_and_fetch(intptrptr, intval);
  intptr = __sync_nand_and_fetch(intptrptr, intval);

  intptr =  __sync_lock_test_and_set(intptrptr, intptr);
  tempbool = __sync_bool_compare_and_swap (intptrptr, intptr, intptr);
  intptr = __sync_val_compare_and_swap (intptrptr, intptr, intptr);
}

void atomic_check_valid_2(int **intptrptr, int *intptr, int intval, _Bool tempbool) {
  intptr = (int *)__sync_fetch_and_add_capability(intptrptr, intval);
  intptr = (int *)__sync_fetch_and_sub_capability(intptrptr, intval);
  intptr = (int *)__sync_add_and_fetch_capability(intptrptr, intval);
  intptr = (int *)__sync_sub_and_fetch_capability(intptrptr, intval);
  intptr = (int *)__sync_fetch_and_and_capability(intptrptr, intval);
  intptr = (int *)__sync_fetch_and_or_capability(intptrptr, intval);
  intptr = (int *)__sync_fetch_and_xor_capability(intptrptr, intval);
  intptr = (int *)__sync_fetch_and_nand_capability(intptrptr, intval);
  intptr = (int *)__sync_and_and_fetch_capability(intptrptr, intval);
  intptr = (int *)__sync_or_and_fetch_capability(intptrptr, intval);
  intptr = (int *)__sync_xor_and_fetch_capability(intptrptr, intval);
  intptr = (int *)__sync_nand_and_fetch_capability(intptrptr, intval);
  intptr = (int *)__sync_fetch_and_add_capability(intptrptr, (__intcap_t)intptr);
  intptr = (int *)__sync_fetch_and_sub_capability(intptrptr, (__intcap_t)intptr);
  intptr = (int *)__sync_add_and_fetch_capability(intptrptr, (__intcap_t)intptr);
  intptr = (int *)__sync_sub_and_fetch_capability(intptrptr, (__intcap_t)intptr);
  intptr = (int *)__sync_fetch_and_and_capability(intptrptr, (__intcap_t)intptr);
  intptr = (int *)__sync_fetch_and_or_capability(intptrptr, (__intcap_t)intptr);
  intptr = (int *)__sync_fetch_and_xor_capability(intptrptr, (__intcap_t)intptr);
  intptr = (int *)__sync_fetch_and_nand_capability(intptrptr, (__intcap_t)intptr);
  intptr = (int *)__sync_and_and_fetch_capability(intptrptr, (__intcap_t)intptr);
  intptr = (int *)__sync_or_and_fetch_capability(intptrptr, (__intcap_t)intptr);
  intptr = (int *)__sync_xor_and_fetch_capability(intptrptr, (__intcap_t)intptr);
  intptr = (int *)__sync_nand_and_fetch_capability(intptrptr, (__intcap_t)intptr);

  intptr =  (int *)__sync_lock_test_and_set_capability(intptrptr, (__intcap_t)intptr);
  tempbool = __sync_bool_compare_and_swap_capability (intptrptr, (__intcap_t)intptr, (__intcap_t)intptr);
  intptr = (int *)__sync_val_compare_and_swap_capability (intptrptr, (__intcap_t)intptr, (__intcap_t)intptr);
}
