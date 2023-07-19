/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** intcap_f:
** 	mov	x0, 1
** 	ret
*/
__intcap intcap_f(void)
{
  return 1;
}

/*
** void_f:
** 	mov	x0, 1
** 	ret
*/
void *void_f(void)
{
  return (void *) 1;
}
