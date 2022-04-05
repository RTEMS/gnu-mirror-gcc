/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */
typedef unsigned long size_t;

size_t
foo (void *__capability c) {
  return __builtin_cheri_perms_get (c);
}

/* { dg-final { scan-assembler-times {gcperm\tx[0-9]+, c[0-9]+} 1 } } */
