/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

const void * __capability
foo (const void * __capability c1, void * __capability c2) {
  return __builtin_cheri_seal(c1, c2);
}

/* { dg-final { scan-assembler-times {seal\tc[0-9]+, c[0-9]+, c[0-9]+} 1 } } */
