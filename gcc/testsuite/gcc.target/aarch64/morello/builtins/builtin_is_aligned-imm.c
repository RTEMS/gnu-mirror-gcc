/* { dg-do compile } */
/* { dg-options "-O2 -march=morello+c64 -mabi=purecap" } */

_Bool
foo_4 (void *c) {
  return __builtin_is_aligned (c, 4);
}

_Bool
foo_16 (void *c) {
  return __builtin_is_aligned (c, 16);
}

_Bool
foo_64 (void *c) {
  return __builtin_is_aligned (c, 64);
}

/* { dg-final { scan-assembler-not {gcvalue\t} } } */
/* { dg-final { scan-assembler-not {scvalue\t} } } */
/* { dg-final { scan-assembler-times {cset\tw[0-9]+, eq} 3 } } */
