/* { dg-do compile } */
/* { dg-options "-O2 -march=morello+c64 -mabi=purecap" } */

bool
foo_4 (void *c) {
  return __builtin_is_aligned (c, 4);
}

bool
foo_16 (void *c) {
  return __builtin_is_aligned (c, 16);
}

bool
foo_64 (void *c) {
  return __builtin_is_aligned (c, 64);
}

/* { dg-final { scan-assembler-not {gcvalue\t} } } */
/* { dg-final { scan-assembler-not {scvalue\t} } } */
/* { dg-final { scan-assembler-times {cset\tw[0-9]+, eq} 3 } } */
