/* { dg-do compile } */
/* { dg-options "-O2 -march=morello+c64 -mabi=purecap" } */

typedef unsigned long size_t;
typedef __INTPTR_TYPE__ intptr_t;

intptr_t
down1 (intptr_t c, size_t alignment) {
  return __builtin_align_down (c, alignment);
}

intptr_t
down2 (size_t alignment) {
  return __builtin_align_down ((intptr_t)128, alignment);
}

intptr_t
down3 () {
  return __builtin_align_down ((intptr_t)128, 16);
}

intptr_t
up1 (intptr_t c, size_t alignment) {
  return __builtin_align_up (c, alignment);
}

intptr_t
up2 (size_t alignment) {
  return __builtin_align_up ((intptr_t)128, alignment);
}

intptr_t
up3 () {
  return __builtin_align_up ((intptr_t)128, 16);
}

bool
isa1 (intptr_t c, size_t alignment) {
  return __builtin_is_aligned (c, alignment);
}

bool
isa2 (size_t alignment) {
  return __builtin_is_aligned ((intptr_t)128, alignment);
}

bool
isa3 () {
  return __builtin_is_aligned ((intptr_t)128, 16);
}

/* { dg-final { scan-assembler-not {gcvalue\t} } } */
