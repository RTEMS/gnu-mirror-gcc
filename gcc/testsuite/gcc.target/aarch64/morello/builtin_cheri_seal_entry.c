/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

void *__capability
foo (void * __capability c) {
  return __builtin_cheri_seal_entry (c);
}

/* { dg-final { scan-assembler-times {seal\tc[0-9]+, c[0-9]+, rb} 1 } } */
