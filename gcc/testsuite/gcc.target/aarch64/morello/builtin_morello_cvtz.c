/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

typedef unsigned long size_t;

void* __capability
bar (void* __capability a, size_t b) {
  return __builtin_morello_cvtz (a, b);
}

/* { dg-final { scan-assembler-times {cvtz\tc[0-9]+, c[0-9]+, x[0-9]+} 1 } } */

