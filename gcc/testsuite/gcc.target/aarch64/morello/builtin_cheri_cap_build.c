/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-march=morello+c64 -mabi=purecap" } */

void * __capability
foo (void *__capability c, char *__capability d) {
  return __builtin_cheri_cap_build (c, (__uintcap_t)d);
}

/* { dg-final { scan-assembler-times {build\tc[0-9]+, c[0-9]+, c[0-9]+} 1 } } */
