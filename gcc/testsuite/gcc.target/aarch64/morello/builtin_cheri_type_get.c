/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

long
foo (void *__capability c) {
  return __builtin_cheri_type_get (c);
}

/* { dg-final { scan-assembler-times {gctype\tx[0-9]+, c[0-9]+} 1 } } */
