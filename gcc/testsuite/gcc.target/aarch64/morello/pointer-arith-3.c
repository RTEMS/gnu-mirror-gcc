/* { dg-do assemble } */
/* { dg-additional-options " -Wno-cheri-explicit-pointer-conversion-from-cap -save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */

/*
** foo:
**	ret
*/
int *foo(int *__capability x) { return (int *)x; }
