/* { dg-do compile } */
/* { dg-require-effective-target cheri_capability_pure } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
**	adr	c0, #0
**	ret
*/
void *
foo (void) {
  return __builtin_cheri_program_counter_get ();
}
