/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

#include <stddef.h> 

size_t
foo (void * __capability cap) {
  return __builtin_cheri_length_get(cap);
}

/* { dg-final { scan-assembler-times {gclen\tx[0-9]+, c[0-9]+} 1 } } */
