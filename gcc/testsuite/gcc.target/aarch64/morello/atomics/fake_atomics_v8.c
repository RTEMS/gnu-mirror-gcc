/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=armv8-a -std=c11" { target { ! cheri_capability_pure } } } */
/* { dg-additional-options "-std=c11" { target { cheri_capability_pure } } } */

#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>

void atomic_check_valid_1(int **intptrptr, int *intptr, int intval,
			  _Bool tempbool, _Bool* tempboolptr, size_t size,
			  void* voidptr)
 {
  intptr = __atomic_load_n(intptrptr, memory_order_relaxed);
  intptr = __atomic_load_n(intptrptr, memory_order_acquire);
  intptr = __atomic_load_n(intptrptr, memory_order_consume);
  intptr = __atomic_load_n(intptrptr, memory_order_seq_cst);

  (void)__atomic_load(intptrptr, intptrptr, memory_order_relaxed);
  (void)__atomic_load(intptrptr, intptrptr, memory_order_acquire);
  (void)__atomic_load(intptrptr, intptrptr, memory_order_consume);
  (void)__atomic_load(intptrptr, intptrptr, memory_order_seq_cst);

  (void)__atomic_store(intptrptr, intptrptr, memory_order_relaxed);
  (void)__atomic_store(intptrptr, intptrptr, memory_order_release);
  (void)__atomic_store(intptrptr, intptrptr, memory_order_seq_cst);

  (void)__atomic_store_n(intptrptr, intptr, memory_order_relaxed);
  (void)__atomic_store_n(intptrptr, intptr, memory_order_release);
  (void)__atomic_store_n(intptrptr, intptr, memory_order_seq_cst);
  (void)__atomic_store_n(intptrptr, intval, memory_order_seq_cst);
  (void)__atomic_store_n(intptrptr, 0, memory_order_seq_cst);
  (void)__atomic_store_n(intptrptr, 1, memory_order_seq_cst);

  intptr = __atomic_fetch_add(intptrptr, intval, memory_order_relaxed);
  intptr = __atomic_fetch_add(intptrptr, intval, memory_order_acquire);
  intptr = __atomic_fetch_add(intptrptr, intval, memory_order_consume);
  intptr = __atomic_fetch_add(intptrptr, intval, memory_order_release);
  intptr = __atomic_fetch_add(intptrptr, intval, memory_order_acq_rel);
  intptr = __atomic_fetch_add(intptrptr, intval, memory_order_seq_cst);
  intptr = __atomic_fetch_add(intptrptr, 0, memory_order_seq_cst);
  intptr = __atomic_fetch_add(intptrptr, 1, memory_order_seq_cst);

  intptr = __atomic_fetch_sub(intptrptr, intval, memory_order_relaxed);
  intptr = __atomic_fetch_sub(intptrptr, intval, memory_order_acquire);
  intptr = __atomic_fetch_sub(intptrptr, intval, memory_order_consume);
  intptr = __atomic_fetch_sub(intptrptr, intval, memory_order_release);
  intptr = __atomic_fetch_sub(intptrptr, intval, memory_order_acq_rel);
  intptr = __atomic_fetch_sub(intptrptr, intval, memory_order_seq_cst);
  intptr = __atomic_fetch_sub(intptrptr, 0, memory_order_seq_cst);
  intptr = __atomic_fetch_sub(intptrptr, 1, memory_order_seq_cst);

  intptr = __atomic_add_fetch(intptrptr, intval, memory_order_relaxed);
  intptr = __atomic_add_fetch(intptrptr, intval, memory_order_acquire);
  intptr = __atomic_add_fetch(intptrptr, intval, memory_order_consume);
  intptr = __atomic_add_fetch(intptrptr, intval, memory_order_release);
  intptr = __atomic_add_fetch(intptrptr, intval, memory_order_acq_rel);
  intptr = __atomic_add_fetch(intptrptr, intval, memory_order_seq_cst);
  intptr = __atomic_add_fetch(intptrptr, 0, memory_order_seq_cst);
  intptr = __atomic_add_fetch(intptrptr, 1, memory_order_seq_cst);

  intptr = __atomic_sub_fetch(intptrptr, intval, memory_order_relaxed);
  intptr = __atomic_sub_fetch(intptrptr, intval, memory_order_acquire);
  intptr = __atomic_sub_fetch(intptrptr, intval, memory_order_consume);
  intptr = __atomic_sub_fetch(intptrptr, intval, memory_order_release);
  intptr = __atomic_sub_fetch(intptrptr, intval, memory_order_acq_rel);
  intptr = __atomic_sub_fetch(intptrptr, intval, memory_order_seq_cst);
  intptr = __atomic_sub_fetch(intptrptr, 0, memory_order_seq_cst);
  intptr = __atomic_sub_fetch(intptrptr, 1, memory_order_seq_cst);

  intptr = __atomic_fetch_and(intptrptr, intval, memory_order_relaxed);
  intptr = __atomic_fetch_and(intptrptr, intval, memory_order_acquire);
  intptr = __atomic_fetch_and(intptrptr, intval, memory_order_consume);
  intptr = __atomic_fetch_and(intptrptr, intval, memory_order_release);
  intptr = __atomic_fetch_and(intptrptr, intval, memory_order_acq_rel);
  intptr = __atomic_fetch_and(intptrptr, intval, memory_order_seq_cst);
  intptr = __atomic_fetch_and(intptrptr, 0, memory_order_seq_cst);
  intptr = __atomic_fetch_and(intptrptr, 1, memory_order_seq_cst);

  intptr = __atomic_fetch_or(intptrptr, intval, memory_order_relaxed);
  intptr = __atomic_fetch_or(intptrptr, intval, memory_order_acquire);
  intptr = __atomic_fetch_or(intptrptr, intval, memory_order_consume);
  intptr = __atomic_fetch_or(intptrptr, intval, memory_order_release);
  intptr = __atomic_fetch_or(intptrptr, intval, memory_order_acq_rel);
  intptr = __atomic_fetch_or(intptrptr, intval, memory_order_seq_cst);
  intptr = __atomic_fetch_or(intptrptr, 0, memory_order_seq_cst);
  intptr = __atomic_fetch_or(intptrptr, 1, memory_order_seq_cst);

  intptr = __atomic_fetch_xor(intptrptr, intval, memory_order_relaxed);
  intptr = __atomic_fetch_xor(intptrptr, intval, memory_order_acquire);
  intptr = __atomic_fetch_xor(intptrptr, intval, memory_order_consume);
  intptr = __atomic_fetch_xor(intptrptr, intval, memory_order_release);
  intptr = __atomic_fetch_xor(intptrptr, intval, memory_order_acq_rel);
  intptr = __atomic_fetch_xor(intptrptr, intval, memory_order_seq_cst);
  intptr = __atomic_fetch_xor(intptrptr, 0, memory_order_seq_cst);
  intptr = __atomic_fetch_xor(intptrptr, 1, memory_order_seq_cst);

  intptr = __atomic_fetch_nand(intptrptr, intval, memory_order_relaxed);
  intptr = __atomic_fetch_nand(intptrptr, intval, memory_order_acquire);
  intptr = __atomic_fetch_nand(intptrptr, intval, memory_order_consume);
  intptr = __atomic_fetch_nand(intptrptr, intval, memory_order_release);
  intptr = __atomic_fetch_nand(intptrptr, intval, memory_order_acq_rel);
  intptr = __atomic_fetch_nand(intptrptr, intval, memory_order_seq_cst);
  intptr = __atomic_fetch_nand(intptrptr, 0, memory_order_seq_cst);
  intptr = __atomic_fetch_nand(intptrptr, 1, memory_order_seq_cst);

  intptr = __atomic_and_fetch(intptrptr, intval, memory_order_relaxed);
  intptr = __atomic_and_fetch(intptrptr, intval, memory_order_acquire);
  intptr = __atomic_and_fetch(intptrptr, intval, memory_order_consume);
  intptr = __atomic_and_fetch(intptrptr, intval, memory_order_release);
  intptr = __atomic_and_fetch(intptrptr, intval, memory_order_acq_rel);
  intptr = __atomic_and_fetch(intptrptr, intval, memory_order_seq_cst);
  intptr = __atomic_and_fetch(intptrptr, 0, memory_order_seq_cst);
  intptr = __atomic_and_fetch(intptrptr, 1, memory_order_seq_cst);

  intptr = __atomic_or_fetch(intptrptr, intval, memory_order_relaxed);
  intptr = __atomic_or_fetch(intptrptr, intval, memory_order_acquire);
  intptr = __atomic_or_fetch(intptrptr, intval, memory_order_consume);
  intptr = __atomic_or_fetch(intptrptr, intval, memory_order_release);
  intptr = __atomic_or_fetch(intptrptr, intval, memory_order_acq_rel);
  intptr = __atomic_or_fetch(intptrptr, intval, memory_order_seq_cst);
  intptr = __atomic_or_fetch(intptrptr, 0, memory_order_seq_cst);
  intptr = __atomic_or_fetch(intptrptr, 1, memory_order_seq_cst);

  intptr = __atomic_xor_fetch(intptrptr, intval, memory_order_relaxed);
  intptr = __atomic_xor_fetch(intptrptr, intval, memory_order_acquire);
  intptr = __atomic_xor_fetch(intptrptr, intval, memory_order_consume);
  intptr = __atomic_xor_fetch(intptrptr, intval, memory_order_release);
  intptr = __atomic_xor_fetch(intptrptr, intval, memory_order_acq_rel);
  intptr = __atomic_xor_fetch(intptrptr, intval, memory_order_seq_cst);
  intptr = __atomic_xor_fetch(intptrptr, 0, memory_order_seq_cst);
  intptr = __atomic_xor_fetch(intptrptr, 1, memory_order_seq_cst);

  intptr = __atomic_nand_fetch(intptrptr, intval, memory_order_relaxed);
  intptr = __atomic_nand_fetch(intptrptr, intval, memory_order_acquire);
  intptr = __atomic_nand_fetch(intptrptr, intval, memory_order_consume);
  intptr = __atomic_nand_fetch(intptrptr, intval, memory_order_release);
  intptr = __atomic_nand_fetch(intptrptr, intval, memory_order_acq_rel);
  intptr = __atomic_nand_fetch(intptrptr, intval, memory_order_seq_cst);
  intptr = __atomic_nand_fetch(intptrptr, 0, memory_order_seq_cst);
  intptr = __atomic_nand_fetch(intptrptr, 1, memory_order_seq_cst);

  (void) __atomic_exchange_n(intptrptr, intptr, memory_order_relaxed);
  (void) __atomic_exchange_n(intptrptr, intptr, memory_order_acquire);
  (void) __atomic_exchange_n(intptrptr, intptr, memory_order_consume);
  (void) __atomic_exchange_n(intptrptr, intptr, memory_order_release);
  (void) __atomic_exchange_n(intptrptr, intptr, memory_order_acq_rel);
  (void) __atomic_exchange_n(intptrptr, intptr, memory_order_seq_cst);
  (void) __atomic_exchange_n(intptrptr, 0, memory_order_seq_cst);
  (void) __atomic_exchange_n(intptrptr, 1, memory_order_seq_cst);

  (void) __atomic_exchange(intptrptr, intptrptr, intptrptr, memory_order_relaxed);
  (void) __atomic_exchange(intptrptr, intptrptr, intptrptr, memory_order_acquire);
  (void) __atomic_exchange(intptrptr, intptrptr, intptrptr, memory_order_consume);
  (void) __atomic_exchange(intptrptr, intptrptr, intptrptr, memory_order_release);
  (void) __atomic_exchange(intptrptr, intptrptr, intptrptr, memory_order_acq_rel);
  (void) __atomic_exchange(intptrptr, intptrptr, intptrptr, memory_order_seq_cst);

  tempbool = __atomic_compare_exchange(intptrptr, intptrptr, intptrptr, 0, memory_order_relaxed, memory_order_relaxed);
  tempbool = __atomic_compare_exchange(intptrptr, intptrptr, intptrptr, 0, memory_order_acquire, memory_order_relaxed);
  tempbool = __atomic_compare_exchange(intptrptr, intptrptr, intptrptr, 0, memory_order_consume, memory_order_relaxed);
  tempbool = __atomic_compare_exchange(intptrptr, intptrptr, intptrptr, 0, memory_order_release, memory_order_relaxed);
  tempbool = __atomic_compare_exchange(intptrptr, intptrptr, intptrptr, 0, memory_order_acq_rel, memory_order_relaxed);
  tempbool = __atomic_compare_exchange(intptrptr, intptrptr, intptrptr, 0, memory_order_seq_cst, memory_order_relaxed);

  tempbool = __atomic_compare_exchange_n(intptrptr, intptrptr, intptr, 0, memory_order_relaxed, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_n(intptrptr, intptrptr, intptr, 0, memory_order_acquire, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_n(intptrptr, intptrptr, intptr, 0, memory_order_consume, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_n(intptrptr, intptrptr, intptr, 0, memory_order_release, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_n(intptrptr, intptrptr, intptr, 0, memory_order_acq_rel, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_n(intptrptr, intptrptr, intptr, 0, memory_order_seq_cst, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_n(intptrptr, intptrptr, 0, 0, memory_order_relaxed, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_n(intptrptr, intptrptr, 1, 0, memory_order_acquire, memory_order_relaxed);

  tempbool = __atomic_test_and_set(voidptr, memory_order_relaxed);
  tempbool = __atomic_test_and_set(voidptr, memory_order_acquire);
  tempbool = __atomic_test_and_set(voidptr, memory_order_consume);
  tempbool = __atomic_test_and_set(voidptr, memory_order_seq_cst);

  (void)__atomic_clear(tempboolptr, memory_order_relaxed);
  (void)__atomic_clear(tempboolptr, memory_order_release);
  (void)__atomic_clear(tempboolptr, memory_order_seq_cst);

  tempbool = __atomic_always_lock_free(sizeof (voidptr), voidptr);
  tempbool = __atomic_always_lock_free(sizeof (voidptr), voidptr);
  tempbool = __atomic_always_lock_free(sizeof (voidptr), voidptr);
  tempbool = __atomic_always_lock_free(sizeof (voidptr), voidptr);
  tempbool = __atomic_is_lock_free(size, voidptr);
  tempbool = __atomic_is_lock_free(size, voidptr);
  tempbool = __atomic_is_lock_free(size, voidptr);
  tempbool = __atomic_is_lock_free(size, voidptr);

}

void atomic_check_valid_2(int **intptrptr, int *intptr, int intval,
			  _Bool tempbool, _Bool* tempboolptr, size_t size,
			  void* voidptr)
 {
  intptr = (int *)  __atomic_load_capability(intptrptr, memory_order_relaxed);
  intptr = (int *)  __atomic_load_capability(intptrptr, memory_order_acquire);
  intptr = (int *)  __atomic_load_capability(intptrptr, memory_order_consume);
  intptr = (int *)  __atomic_load_capability(intptrptr, memory_order_seq_cst);

  (void)__atomic_store_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  (void)__atomic_store_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  (void)__atomic_store_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);

  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, intval, memory_order_relaxed);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, intval, memory_order_acquire);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, intval, memory_order_consume);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, intval, memory_order_release);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, intval, memory_order_acq_rel);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, intval, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, 0, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_add_capability(intptrptr, 1, memory_order_seq_cst);

  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, intval, memory_order_relaxed);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, intval, memory_order_acquire);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, intval, memory_order_consume);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, intval, memory_order_release);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, intval, memory_order_acq_rel);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, intval, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, 0, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_sub_capability(intptrptr, 1, memory_order_seq_cst);

  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, intval, memory_order_relaxed);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, intval, memory_order_acquire);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, intval, memory_order_consume);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, intval, memory_order_release);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, intval, memory_order_acq_rel);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, intval, memory_order_seq_cst);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, 0, memory_order_seq_cst);
  intptr = (int *)  __atomic_add_fetch_capability(intptrptr, 1, memory_order_seq_cst);

  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, intval, memory_order_relaxed);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, intval, memory_order_acquire);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, intval, memory_order_consume);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, intval, memory_order_release);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, intval, memory_order_acq_rel);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, intval, memory_order_seq_cst);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, 0, memory_order_seq_cst);
  intptr = (int *)  __atomic_sub_fetch_capability(intptrptr, 1, memory_order_seq_cst);

  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, intval, memory_order_relaxed);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, intval, memory_order_acquire);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, intval, memory_order_consume);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, intval, memory_order_release);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, intval, memory_order_acq_rel);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, intval, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, 0, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_and_capability(intptrptr, 1, memory_order_seq_cst);

  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, intval, memory_order_relaxed);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, intval, memory_order_acquire);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, intval, memory_order_consume);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, intval, memory_order_release);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, intval, memory_order_acq_rel);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, intval, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, 0, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_or_capability(intptrptr, 1, memory_order_seq_cst);

  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, intval, memory_order_relaxed);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, intval, memory_order_acquire);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, intval, memory_order_consume);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, intval, memory_order_release);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, intval, memory_order_acq_rel);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, intval, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, 0, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_xor_capability(intptrptr, 1, memory_order_seq_cst);

  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, intval, memory_order_relaxed);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, intval, memory_order_acquire);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, intval, memory_order_consume);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, intval, memory_order_release);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, intval, memory_order_acq_rel);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, intval, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, 0, memory_order_seq_cst);
  intptr = (int *)  __atomic_fetch_nand_capability(intptrptr, 1, memory_order_seq_cst);

  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, intval, memory_order_relaxed);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, intval, memory_order_acquire);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, intval, memory_order_consume);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, intval, memory_order_release);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, intval, memory_order_acq_rel);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, intval, memory_order_seq_cst);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, 0, memory_order_seq_cst);
  intptr = (int *)  __atomic_and_fetch_capability(intptrptr, 1, memory_order_seq_cst);

  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, intval, memory_order_relaxed);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, intval, memory_order_acquire);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, intval, memory_order_consume);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, intval, memory_order_release);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, intval, memory_order_acq_rel);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, intval, memory_order_seq_cst);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, 0, memory_order_seq_cst);
  intptr = (int *)  __atomic_or_fetch_capability(intptrptr, 1, memory_order_seq_cst);

  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, intval, memory_order_relaxed);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, intval, memory_order_acquire);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, intval, memory_order_consume);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, intval, memory_order_release);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, intval, memory_order_acq_rel);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, intval, memory_order_seq_cst);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, 0, memory_order_seq_cst);
  intptr = (int *)  __atomic_xor_fetch_capability(intptrptr, 1, memory_order_seq_cst);

  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, intval, memory_order_relaxed);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, intval, memory_order_acquire);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, intval, memory_order_consume);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, intval, memory_order_release);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, intval, memory_order_acq_rel);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, intval, memory_order_seq_cst);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, 0, memory_order_seq_cst);
  intptr = (int *)  __atomic_nand_fetch_capability(intptrptr, 1, memory_order_seq_cst);

  (void) __atomic_exchange_capability(intptrptr, (__intcap_t) intptr, memory_order_relaxed);
  (void) __atomic_exchange_capability(intptrptr, (__intcap_t) intptr, memory_order_acquire);
  (void) __atomic_exchange_capability(intptrptr, (__intcap_t) intptr, memory_order_consume);
  (void) __atomic_exchange_capability(intptrptr, (__intcap_t) intptr, memory_order_release);
  (void) __atomic_exchange_capability(intptrptr, (__intcap_t) intptr, memory_order_acq_rel);
  (void) __atomic_exchange_capability(intptrptr, (__intcap_t) intptr, memory_order_seq_cst);
  (void) __atomic_exchange_capability(intptrptr, 0, memory_order_seq_cst);
  (void) __atomic_exchange_capability(intptrptr, 1, memory_order_seq_cst);

  tempbool = __atomic_compare_exchange_capability(intptrptr, intptrptr, (__intcap_t) intptr, 0, memory_order_relaxed, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_capability(intptrptr, intptrptr, (__intcap_t) intptr, 0, memory_order_acquire, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_capability(intptrptr, intptrptr, (__intcap_t) intptr, 0, memory_order_consume, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_capability(intptrptr, intptrptr, (__intcap_t) intptr, 0, memory_order_release, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_capability(intptrptr, intptrptr, (__intcap_t) intptr, 0, memory_order_acq_rel, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_capability(intptrptr, intptrptr, (__intcap_t) intptr, 0, memory_order_seq_cst, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_capability(intptrptr, intptrptr, 0, 0, memory_order_seq_cst, memory_order_relaxed);
  tempbool = __atomic_compare_exchange_capability(intptrptr, intptrptr, 1, 0, memory_order_seq_cst, memory_order_relaxed);
}
