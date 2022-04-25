/* { dg-do compile } */

#include <stdatomic.h>

void atomic_check_valid_1(int *__capability *intptrptr, int *intptr)
{
  intptr = __atomic_fetch_add(intptrptr, intptr, memory_order_relaxed); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __atomic_fetch_sub(intptrptr, intptr, memory_order_relaxed); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __atomic_add_fetch(intptrptr, intptr, memory_order_relaxed); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __atomic_sub_fetch(intptrptr, intptr, memory_order_relaxed); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __atomic_fetch_and(intptrptr, intptr, memory_order_relaxed); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __atomic_fetch_or(intptrptr, intptr, memory_order_relaxed); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __atomic_fetch_xor(intptrptr, intptr, memory_order_relaxed); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __atomic_fetch_nand(intptrptr, intptr, memory_order_relaxed); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __atomic_and_fetch(intptrptr, intptr, memory_order_relaxed); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __atomic_or_fetch(intptrptr, intptr, memory_order_relaxed); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __atomic_xor_fetch(intptrptr, intptr, memory_order_relaxed); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __atomic_nand_fetch(intptrptr, intptr, memory_order_relaxed); /* { dg-warning "integer from pointer without a cast" } */
}
