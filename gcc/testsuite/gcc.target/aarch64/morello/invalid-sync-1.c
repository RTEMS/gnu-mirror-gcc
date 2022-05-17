/* { dg-do compile } */
/* { dg-additional-options "-Wno-cheri-implicit-pointer-conversion-from-cap" } */

void atomic_check_valid_1(int *__capability *intptrptr, int *intptr) {
  intptr = __sync_fetch_and_add(intptrptr, intptr); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __sync_fetch_and_sub(intptrptr, intptr); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __sync_add_and_fetch(intptrptr, intptr); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __sync_sub_and_fetch(intptrptr, intptr); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __sync_fetch_and_and(intptrptr, intptr); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __sync_fetch_and_or(intptrptr, intptr); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __sync_fetch_and_xor(intptrptr, intptr); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __sync_fetch_and_nand(intptrptr, intptr); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __sync_and_and_fetch(intptrptr, intptr); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __sync_or_and_fetch(intptrptr, intptr); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __sync_xor_and_fetch(intptrptr, intptr); /* { dg-warning "integer from pointer without a cast" } */
  intptr = __sync_nand_and_fetch(intptrptr, intptr); /* { dg-warning "integer from pointer without a cast" } */
}
