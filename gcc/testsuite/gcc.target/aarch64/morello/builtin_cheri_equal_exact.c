/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

_Bool
foo (void *__capability c, void *__capability d) {
  return __builtin_cheri_equal_exact (c, d);
}

/* { dg-final { scan-assembler-times {chkeq\tc[0-9]+, c[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {cset} 1 } } */
