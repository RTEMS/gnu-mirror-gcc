/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

typedef long long vaddr_t;

vaddr_t
foo (void * __capability cap) {
  return __builtin_cheri_base_get (cap);
}

/* { dg-final { scan-assembler-times {gcbase\tx[0-9]+, c[0-9]+} 1 } } */
