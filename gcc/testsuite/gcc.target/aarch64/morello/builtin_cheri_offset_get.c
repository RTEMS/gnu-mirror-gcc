/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

#include <stddef.h>

size_t
foo (void * __capability c) {
  return __builtin_cheri_offset_get(c);
}

/* { dg-final { scan-assembler-times {gcoff\tx[0-9]+, c[0-9]+} 1 } } */
