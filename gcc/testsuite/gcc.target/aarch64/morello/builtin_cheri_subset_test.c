/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

_Bool
foo (void *__capability c, void *__capability d) {
  return __builtin_cheri_subset_test (c, d);
}

/* { dg-final { scan-assembler-times {chkss\tc[0-9]+, c[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {cset} 1 } } */
