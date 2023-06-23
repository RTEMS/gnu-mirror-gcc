/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#define N 640
int a[N] = {0};
int b[N] = {0};

/*
** f1:
**	...
**	vcmp.s32	gt, q[0-9]+, q[0-9]+
**	vmrs	r[0-9]+, p0	@ movhi
**	cbnz	r[0-9]+, \.L[0-9]+
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
**	vcmp.s32	ge, q[0-9]+, q[0-9]+
**	vmrs	r[0-9]+, p0	@ movhi
**	cbnz	r[0-9]+, \.L[0-9]+
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
**	vcmp.i32	eq, q[0-9]+, q[0-9]+
**	vmrs	r[0-9]+, p0	@ movhi
**	cbnz	r[0-9]+, \.L[0-9]+
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
**	vcmp.i32	ne, q[0-9]+, q[0-9]+
**	vmrs	r[0-9]+, p0	@ movhi
**	cbnz	r[0-9]+, \.L[0-9]+
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
**	vcmp.s32	lt, q[0-9]+, q[0-9]+
**	vmrs	r[0-9]+, p0	@ movhi
**	cbnz	r[0-9]+, \.L[0-9]+
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
**	vcmp.s32	le, q[0-9]+, q[0-9]+
**	vmrs	r[0-9]+, p0	@ movhi
**	cbnz	r[0-9]+, \.L[0-9]+
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
