/* Test -Wno-pointer-to-int-cast.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wno-pointer-to-int-cast" } */

void *p;

char
f (void)
{
  return (char) p;
}


char c;

void *
g (void)
{
  return (void *) c; /* { dg-warning "cast to pointer from integer of different size" } */
  /* { dg-warning "cast from provenance-free integer type to pointer type will give pointer that can not be dereferenced" "" { target { cheri_pointers_are_caps } } .-1 } */
}
