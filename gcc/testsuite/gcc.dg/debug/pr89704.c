/* PR debug/89704 */
/* { dg-do compile } */

typedef __INTPTR_TYPE__ intptr_t;

int
foo (void)
{
  lab1:;
  lab2:;
  static int i = (intptr_t) &&lab1 - (intptr_t) &&lab2;
  /* { dg-warning "binary expression on capability types" "" { target { aarch64_capability_any } } .-1 } */
  static int j = (intptr_t) &&lab1 - (intptr_t) &&lab2;
  /* { dg-warning "binary expression on capability types" "" { target { aarch64_capability_any } } .-1 } */
  return i;
}
