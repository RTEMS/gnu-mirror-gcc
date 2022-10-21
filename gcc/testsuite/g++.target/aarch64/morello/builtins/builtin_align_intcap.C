/* { dg-do compile } */
/* { dg-options "-O2 -march=morello+c64 -mabi=purecap" } */

typedef unsigned long size_t;

__intcap_t
down1 (__intcap_t c, size_t alignment) {
  return __builtin_align_down (c, alignment);
}

__intcap_t
down2 (size_t alignment) {
  return __builtin_align_down ((__intcap_t)128, alignment);
}

__intcap_t
down3 (size_t alignment) {
  return __builtin_align_down ((__intcap_t)128, 16);
}

__intcap_t
up1 (__intcap_t c, size_t alignment) {
  return __builtin_align_up (c, alignment);
}

__intcap_t
up2 (size_t alignment) {
  return __builtin_align_up ((__intcap_t)128, alignment);
}

__intcap_t
up3 (size_t alignment) {
  return __builtin_align_up ((__intcap_t)128, 16);
}

bool
isa1 (__intcap_t c, size_t alignment) {
  return __builtin_is_aligned (c, alignment);
}

bool
isa2 (size_t alignment) {
  return __builtin_is_aligned ((__intcap_t)128, alignment);
}

bool
isa3 (size_t alignment) {
  return __builtin_is_aligned ((__intcap_t)128, 16);
}

/* { dg-final { scan-assembler-not {gcvalue\t} } } */
