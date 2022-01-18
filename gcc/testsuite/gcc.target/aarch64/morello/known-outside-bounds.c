/* { dg-do compile { target aarch64*-*-* } } */
static const char *const myvar = "" + 10;
const char *
foo ()
{
  return myvar;
} /* { dg-warning "offset is outside of .* capability from which it is offset" "" { target *-*-* } } */

