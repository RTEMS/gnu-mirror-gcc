/* Test for warnings for non-string-literal formats.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat -Wformat-nonliteral" } */

#include "format.h"

void
foo (char *s, size_t i)
{
  printf ((const char *)i, i); /* { dg-warning "argument types" "non-literal" } */
  /* { dg-warning "cast from provenance-free integer type to pointer type will give pointer that can not be dereferenced" "" { target { cheri_pointers_are_caps } } .-1 } */
  printf (s, i); /* { dg-warning "argument types" "non-literal" } */
}
