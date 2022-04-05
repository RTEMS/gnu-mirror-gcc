/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */
#include <stdbool.h>

bool
foo (void *__capability c) {
  return __builtin_cheri_tag_get (c);
}

/* { dg-final { scan-assembler-times {gctag\tx[0-9]+, c[0-9]+} 1 } } */
