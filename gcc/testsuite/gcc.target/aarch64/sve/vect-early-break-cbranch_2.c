/* { dg-do compile } */
/* { dg-options "-O3 --param=aarch64-autovec-preference=1" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#define N 640
int a[N] = {0};
int b[N] = {0};


/*
** f1:
**	...
**	cmgt	v[0-9]+.4s, v[0-9]+.4s, #0
**	cmpne	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0
**	b.any	\.L[0-9]+
**	...
*/
void f1 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] > 0)
	break;
    }
}

/*
** f2:
**	...
**	cmge	v[0-9]+.4s, v[0-9]+.4s, #0
**	cmpne	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0
**	b.any	\.L[0-9]+
**	...
*/
void f2 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] >= 0)
	break;
    }
}

/*
** f3:
**	...
**	cmpeq	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, z[0-9]+.s
**	b.any	\.L[0-9]+
**	...
*/
void f3 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] == 0)
	break;
    }
}

/*
** f4:
**	...
**	cmpne	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, z[0-9]+.s
**	b.any	\.L[0-9]+
**	...
*/
void f4 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] != 0)
	break;
    }
}

/*
** f5:
**	...
**	cmlt	v[0-9]+.4s, v[0-9]+.4s, #0
**	cmpne	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0
**	b.any	\.L[0-9]+
**	...
*/
void f5 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] < 0)
	break;
    }
}

/*
** f6:
**	...
**	cmle	v[0-9]+.4s, v[0-9]+.4s, #0
**	cmpne	p[0-9]+.s, p[0-9]+/z, z[0-9]+.s, #0
**	b.any	\.L[0-9]+
**	...
*/
void f6 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] <= 0)
	break;
    }
}
