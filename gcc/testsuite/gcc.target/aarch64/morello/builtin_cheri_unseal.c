/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

void *__capability
foo (void *__capability c, void *__capability d) {
  return __builtin_cheri_unseal (c, d);
}

/* { dg-final { scan-assembler-times {unseal\tc[0-9]+, c[0-9]+, c[0-9]+} 1 } } */
