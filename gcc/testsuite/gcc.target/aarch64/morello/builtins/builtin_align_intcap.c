/* { dg-do compile } */
/* { dg-require-effective-target cheri_capability_any } */

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
down3 () {
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
up3 () {
  return __builtin_align_up ((__intcap_t)128, 16);
}

_Bool
isa1 (__intcap_t c, size_t alignment) {
  return __builtin_is_aligned (c, alignment);
}

_Bool
isa2 (size_t alignment) {
  return __builtin_is_aligned ((__intcap_t)128, alignment);
}

_Bool
isa3 () {
  return __builtin_is_aligned ((__intcap_t)128, 16);
}

/* { dg-final { scan-assembler-not {gcvalue\t} } } */
