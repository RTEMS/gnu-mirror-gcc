/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

#include <stddef.h>

void * __capability
foo (void * __capability c, size_t x) {
  return __builtin_cheri_perms_and(c, x);
}

/* { dg-final { scan-assembler-times {mvn\tx[0-9]+, x[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {clrperm\tc[0-9]+, c[0-9]+, x[0-9]+} 1 } } */
