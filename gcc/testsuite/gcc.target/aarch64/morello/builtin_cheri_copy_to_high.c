/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */
#include <stddef.h>

void *__capability
foo (void * __capability c, size_t x) {
  return __builtin_cheri_copy_to_high (c, x);
}

/* { dg-final { scan-assembler-times {cthi\tc[0-9]+, c[0-9]+, x[0-9]+} 1 } } */
