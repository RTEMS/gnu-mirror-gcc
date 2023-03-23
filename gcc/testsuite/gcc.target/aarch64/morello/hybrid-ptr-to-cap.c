/* { dg-do compile } */
/* { dg-require-effective-target cheri_capability_hybrid } */
/* { dg-final { check-function-bodies "**" "" { {-O[123s]} } } } */

/*
** foo:
**	cvtdz	c0, x0
**	ret
*/
void * __capability foo(void *p)
{
  return (void * __capability)p; /* { dg-warning "cast from non-capability pointer" } */
}
