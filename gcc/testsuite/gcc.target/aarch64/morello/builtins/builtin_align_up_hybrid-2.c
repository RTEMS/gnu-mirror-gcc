/* { dg-do compile } */
/* { dg-options "-O2 -march=morello -mabi=lp64" } */

typedef unsigned long size_t;

void *
foo (void *c) {
  return __builtin_align_up (c, 16);
}

void *
bar (void *c, size_t alignment) {
  return __builtin_align_up (c, alignment);
}

int
baz (size_t alignment) {
  return __builtin_align_up (128, alignment);
}

/* { dg-final { scan-assembler-not {gcvalue\t} } } */
/* { dg-final { scan-assembler-not {scvalue\t} } } */
/* { dg-final { scan-assembler-not {alignu\t} } } */
/* { dg-final { scan-assembler-not {alignd\t} } } */
