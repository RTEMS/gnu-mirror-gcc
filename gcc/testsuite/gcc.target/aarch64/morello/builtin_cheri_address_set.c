/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

typedef long long vaddr_t;

void * __capability
bar (void * __capability c, vaddr_t s) {
  return __builtin_cheri_address_set (c, s);
}

/* { dg-final { scan-assembler-times {scvalue\tc[0-9]+, c[0-9]+, x[0-9]+} 1 } } */
