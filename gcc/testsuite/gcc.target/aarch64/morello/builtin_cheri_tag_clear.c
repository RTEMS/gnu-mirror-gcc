/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

void *__capability
foo (void * __capability c) {
  return __builtin_cheri_tag_clear (c);
}

/* { dg-final { scan-assembler-times {clrtag\tc[0-9]+, c[0-9]+} 1 } } */
