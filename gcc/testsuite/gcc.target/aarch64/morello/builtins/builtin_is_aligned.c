/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -march=morello+c64 -mabi=purecap" } */

typedef unsigned long size_t;

_Bool
foo2 (size_t a) {
  return __builtin_is_aligned (128, a);
}

_Bool
foo3 (void * c) {
  return __builtin_is_aligned (c, 16);
}

_Bool
bar (void * c, size_t a) {
  return __builtin_is_aligned (c, a);
}

_Bool
baz (void * c, size_t *a) {
  return __builtin_is_aligned (c, *a);
}

/* { dg-final { scan-assembler-not {gcvalue\t} } } */
/* { dg-final { scan-assembler-not {scvalue\t} } } */
/* { dg-final { scan-assembler-times {cset\tw[0-9], eq} 4 } } */
