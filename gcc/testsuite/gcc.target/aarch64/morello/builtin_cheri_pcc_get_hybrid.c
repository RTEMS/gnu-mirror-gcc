/* { dg-do compile } */
/* { dg-require-effective-target cheri_capability_hybrid } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
**	adr	x0, #0
**	cvtp	c0, x0
**	ret
*/
void *__capability
foo (void) {
  return __builtin_cheri_program_counter_get ();
}
