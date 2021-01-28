// { dg-do compile }
// { dg-options "-Wno-size_t-literals" }

#include <stddef.h>

size_t s1 = 1234zu;
size_t s2 = 1234uz;

ptrdiff_t pd1 = 1234z;
