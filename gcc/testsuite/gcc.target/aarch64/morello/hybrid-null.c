/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
** 	mov	x0, xzr
** 	ret
*/
char * __capability foo(void)
{
  return (void *)0;
}
