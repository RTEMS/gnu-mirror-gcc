/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -pedantic-errors" } */
enum {
  e1 = 1 || ((__intcap_t)1, 1),
  e2 = 1 || (1, (__intcap_t)1),
  e3 = (__intcap_t)1 || (1, 1),
  e4 = (__intcap_t)1 || ((__intcap_t)1, (__intcap_t)1),
  e5 = 1 || (1, ((__intcap_t)1 + 1)),
  e6 = 0 && (1, (__intcap_t)1),
  e7 = 1 || (1 + ((__intcap_t)1, 1)),
  e8 = (__intcap_t)1 || (1, 1),
  e9 = 1 || ((__intcap_t)1 + (1, 1)),
};
