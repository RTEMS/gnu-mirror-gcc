/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap -fno-cheri-stack-bounds" } */

#include <stddef.h>

void * __capability
foo (void * __capability c, size_t x) {
  return __builtin_cheri_bounds_set_exact(c, x);
}

/* { dg-final { scan-assembler-times {scbndse\tc[0-9]+, c[0-9]+, x[0-9]+} 1 } } */
