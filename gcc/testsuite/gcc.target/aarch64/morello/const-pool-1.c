/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  {-O[123s]} {target aarch64_small} } } */

/*
** get_double:
**	adrp	([xc][0-9]+), \.LC0
**	ldr	d0, \[\1, #:lo12:\.LC0\]
**	ret
*/
double get_double () { return 1.2345; }

/* { dg-final { scan-assembler-not {\.LC1:} } } */
