/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** intcap_f1:
**	mov	x0, 1
**	movk	x0, 0x2, lsl 16
**	ret
*/
__intcap intcap_f1(void)
{
  return 0x2L << 16 | 0x1L << 0;
}

/*
** intcap_f2:
** 	mov	x0, 1
**	movk	x0, 0x2, lsl 16
**	movk	x0, 0x3, lsl 32
**	ret
*/
__intcap intcap_f2(void)
{
  return 0x3L << 32 | 0x2L << 16 | 0x1L << 0;
}

/*
** intcap_f3:
**	mov	x0, 1
**	movk	x0, 0x2, lsl 16
**	movk	x0, 0x3, lsl 32
**	movk	x0, 0x4, lsl 48
**	ret
*/
__intcap intcap_f3(void)
{
  return 0x4L << 48 | 0x3L << 32 | 0x2L << 16 | 0x1L << 0;
}

/*
** intcap_f_bitmask_imm:
**	mov	w0, 2147483649
**	ret
*/
__intcap intcap_f_bitmask_imm(void)
{
  return 0x80000001;
}
