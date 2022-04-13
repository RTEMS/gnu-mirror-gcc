/* { dg-do assemble } */
/* { dg-additional-options "-mpc-relative-literal-loads -save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } { target cheri_capability_pure } } } */

static int x;

/*
** get_ptr:
**	adrp	(c[0-9]+), \.LANCHOR0
**	ldr	c0, \[\1, #:lo12:\.LANCHOR0\]
**	ret
*/
int *get_ptr () { return &x; }

/* { dg-final { scan-assembler {\t\.data\.rel\.ro} { target cheri_capability_pure } } } */
