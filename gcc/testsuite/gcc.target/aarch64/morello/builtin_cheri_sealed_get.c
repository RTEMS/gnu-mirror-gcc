/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

_Bool
foo (void *__capability c) {
  return __builtin_cheri_sealed_get (c);
}

/* { dg-final { scan-assembler-times {gcseal\tx[0-9]+, c[0-9]+} 1 } } */
