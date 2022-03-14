/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

typedef long long vaddr_t;

vaddr_t
baz (vaddr_t len) {
  return __builtin_cheri_representable_alignment_mask (len);
}

/* { dg-final { scan-assembler-times {rrmask\tx[0-9]+, x[0-9]+} 1 } } */
