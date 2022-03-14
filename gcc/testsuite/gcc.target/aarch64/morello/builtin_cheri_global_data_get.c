/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

void * __capability
foo (void) {
  return __builtin_cheri_global_data_get();
}

/* { dg-final { scan-assembler-times {mrs\tc[0-9]+, DDC} 1 } } */
