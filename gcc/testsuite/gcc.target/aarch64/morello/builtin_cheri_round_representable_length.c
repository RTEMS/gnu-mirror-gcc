/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

typedef long long vaddr_t;

vaddr_t
foo (vaddr_t len) {
  return __builtin_cheri_round_representable_length (len);
}

/* { dg-final { scan-assembler-times {rrlen\tx[0-9]+, x[0-9]+} 1 } } */
