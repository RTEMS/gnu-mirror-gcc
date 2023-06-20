/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#pragma GCC target "+nosve"

#define N 640
int a[N] = {0};
int b[N] = {0};


/*
** f1:
**	...
	cmeq	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
	uminp	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
	fmov	x[0-9]+, d[0-9]+
	cbz	x[0-9]+, \.L[0-9]+
**	...
*/
void f1 (int x)
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] != x)
	break;
    }
}
