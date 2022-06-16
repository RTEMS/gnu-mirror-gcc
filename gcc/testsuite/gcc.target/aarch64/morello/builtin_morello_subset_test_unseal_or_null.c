/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

void *__capability
foo (const void *__capability a, const void *__capability b) {
  return __builtin_morello_subset_test_unseal_or_null (a, b);
}

/* { dg-final { scan-assembler-times {chkssu\tc[0-9]+, c[0-9]+, c[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {csel\t} 1 } } */
