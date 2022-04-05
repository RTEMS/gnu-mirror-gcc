/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */
typedef unsigned long size_t;

void *__capability
foo (void *__capability c, size_t x) {
  return __builtin_cheri_flags_set (c, x);
}

/* { dg-final { scan-assembler-times {scflgs\tc[0-9]+, c[0-9]+, x[0-9]+} 1 } } */
