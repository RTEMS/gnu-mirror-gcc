// { dg-do compile }
// { dg-options "-Wno-size_t-literals" }

#include <stddef.h>

size_t s1 = 1234zu;
size_t S1 = 5678ZU;
size_t s2 = 1234uz;
size_t S2 = 5678UZ;

ptrdiff_t pd1 = 1234z;
ptrdiff_t PD1 = 5678Z;
