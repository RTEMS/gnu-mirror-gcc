/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

void * __capability
foo (void* __capability a, void* __capability b) {
  return __builtin_morello_chkssu (a, b);
}

/* { dg-final { scan-assembler-times {chkssu\tc[0-9]+, c[0-9]+, c[0-9]+} 1 } } */
