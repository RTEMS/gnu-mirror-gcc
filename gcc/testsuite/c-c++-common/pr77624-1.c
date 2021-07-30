/* PR middle-end/77624 */
/* { dg-do compile } */

int
foo (int a)
{
  return __atomic_is_lock_free (2, a);		/* { dg-warning "pointer from integer" "" { target { c && { ! aarch64_capability_any } } } } */
}						/* { dg-error "invalid conversion" "" { target c++ } .-1 } */
/* { dg-error "passing 'int' to parameter of incompatible type capability" "" { target { c && { aarch64_capability_any } } } .-2 } */

int
bar (int a)
{
  return __atomic_always_lock_free (2, a);	/* { dg-warning "pointer from integer" "" { target { c && { ! aarch64_capability_any } } } } */
}						/* { dg-error "invalid conversion" "" { target c++ } .-1 } */
/* { dg-error "passing 'int' to parameter of incompatible type capability" "" { target { c && { aarch64_capability_any } } } .-2 } */

