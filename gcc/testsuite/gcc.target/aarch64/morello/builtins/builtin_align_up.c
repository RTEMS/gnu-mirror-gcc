/* { dg-do compile } */
/* { dg-options "-O2 -march=morello+c64 -mabi=purecap" } */

typedef unsigned long size_t;

void *
foo2 (void *c, size_t alignment) {
  return __builtin_align_up (c, 16);
}

int
foo3 (size_t alignment) {
  return __builtin_align_up (128, alignment);
}

void *
bar (void *c, size_t alignment) {
  return __builtin_align_up (c, alignment);
}

void *
baz (void *c, size_t *alignment) {
  return __builtin_align_up (c, *alignment);
}

/* { dg-final { scan-assembler-not {gcvalue\t} } } */
/* { dg-final { scan-assembler-times {alignu\t} 1 } } */
/* { dg-final { scan-assembler-times {scvalue\tc[0-9]+, c[0-9]+, x[0-9]+} 2 } } */
