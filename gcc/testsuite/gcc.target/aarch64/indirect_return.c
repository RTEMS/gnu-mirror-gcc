/* { dg-do compile } */
/* { dg-options "-mbranch-protection=bti" } */

int __attribute((indirect_return))
foo (int a)
{
  return a;
}

/*
**func1:
**	hint	34 // bti c
**	...
**	bl	foo
**	hint	36 // bti j
**	...
**	ret
*/
int
func1 (int a, int b)
{
  return foo (a + b);
}

/* { dg-final { check-function-bodies "**" "" "" } } */
