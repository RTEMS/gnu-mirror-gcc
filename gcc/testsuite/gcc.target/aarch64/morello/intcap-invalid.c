/* { dg-do compile } */
void unary_plus_non_lvalue(__intcap c)
{
  +c = c; /* { dg-error "lvalue required as left operand of assignment" } */
}
