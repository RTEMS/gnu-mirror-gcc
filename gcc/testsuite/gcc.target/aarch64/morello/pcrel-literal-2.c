/* { dg-do assemble } */
/* { dg-additional-options "-mpc-relative-literal-loads -save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */

/*
** get_double:
**	ldr	d0, \.LC0
**	ret
*/
double get_double () { return 1.2345; }

/* { dg-final { scan-assembler-not {\t\.data} } } */
