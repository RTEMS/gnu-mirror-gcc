/* { dg-do assemble } */
/* { dg-additional-options "-mcmodel=large -fno-PIC -fgimple -save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } }  */
/* { dg-require-effective-target cheri_capability_hybrid } */

/*
** foo:
**	adrp	(x[0-9]+), \.LC1
**	ldr	(x[0-9]+), \[\1, #:lo12:\.LC1\]
**	ldr	c0, \[\2\]
**	ret
*/
int x;
__GIMPLE int *__capability foo() {
  int *__capability res;

  res = __CAP_ADDR x;
  return res;
}

/* { dg-final { scan-assembler {\t\.size\t\.LC1, 8\n\.LC1:\n\t\.xword\t\.LC0\n} } } */
/* { dg-final { scan-assembler {\t\.section\t\.data\.rel\.ro\.local,"aw"\n\t\.align\t4\n\t\.type\t\.LC0, %object\n} } } */
/* { dg-final { scan-assembler {\t\.size\t\.LC0, 16\n\.LC0:\n\t\.chericap\tx\n} } } */
