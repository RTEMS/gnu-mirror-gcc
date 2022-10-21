/* { dg-do compile } */
/* { dg-options "-O2 -march=morello -mabi=lp64" } */

typedef unsigned long size_t;

void * __capability
foo1 (void * __capability c, size_t alignment) {
  return __builtin_align_up (c, alignment);
}

void * __capability
foo2 (void * __capability c) {
  return __builtin_align_up (c, 16);
}

/* { dg-final { scan-assembler-not {gcvalue\t} } } */
/* { dg-final { scan-assembler-times {alignu\t} 1 } } */
/* { dg-final { scan-assembler-times {scvalue\tc[0-9]+, c[0-9]+, x[0-9]+} 1 } } */
