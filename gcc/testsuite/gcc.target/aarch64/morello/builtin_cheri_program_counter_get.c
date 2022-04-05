/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

void *__capability
foo (void) {
  return __builtin_cheri_program_counter_get ();
}

/* { dg-final { scan-assembler-times {adr\tc[0-9]+, #0} 1 } } */
