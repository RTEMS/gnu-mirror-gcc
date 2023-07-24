/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" { {-O[23s]} } } } */
/* { dg-require-effective-target cheri_capability_any } */

/*
** ptr_f:
**	cmp	w0, 0
**	csel	c0, c2, c1, eq
**	ret
*/
char * __capability ptr_f(int x, char * __capability p, char * __capability q)
{
    return x ? p : q;
}

/*
** intcap_f:
**	cmp	w0, 0
**	csel	c0, c1, c2, ne
**	ret
*/
__intcap intcap_f(int x, __intcap p, __intcap q)
{
    return x ? p : q;
}

/*
** intcap_f_zero1:
**	cmp	w0, 0
**	csel	c0, c1, czr, eq
**	ret
*/
__intcap intcap_f_zero1(int x, __intcap p)
{
    return x ? 0 : p;
}

/*
** intcap_f_zero2:
**	cmp	w0, 0
**	csel	c0, c1, czr, ne
**	ret
*/
__intcap intcap_f_zero2(int x, __intcap p)
{
    return x ? p : 0;
}

/*
** intcap_f_1:
**	cmp	w0, 0
**	mov	x([0-9]+), 1
**	csel	c0, c1, c\1, ne
**	ret
*/
__intcap intcap_f_1(int x, __intcap p)
{
    return x ? p : 1;
}

/*
** intcap_f_m1:
**	cmp	w0, 0
**	mov	x([0-9]+), -1
**	csel	c0, c1, c\1, ne
**	ret
*/
__intcap intcap_f_m1(int x, __intcap p)
{
    return x ? p : -1;
}