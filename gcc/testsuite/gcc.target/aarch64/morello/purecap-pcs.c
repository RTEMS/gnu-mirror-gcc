/* { dg-do compile } */
/* { dg-options "-march=morello+c64 -mabi=purecap -O2" } */

void g(void);

/*
** foo:
**	stp	c29, c30, \[csp, -32\]!
**	mov	c29, csp
**	bl	g
**	ldp	c29, c30, \[csp\], 32
**	b	g
*/
void foo(void) { g(); g(); }


/* Check save/restore of callee-saved regs.  */

/*
** clob1:
**	str	c19, \[csp, -16\]!
**	ldr	c19, \[csp\], 16
**	ret
*/
void clob1(void)
{
  asm("" ::: "c19");
}

/*
** clob2:
**	stp	c19, c20, \[csp, -32\]!
**	ldp	c19, c20, \[csp\], 32
**	ret
*/
void clob2(void)
{
  asm("" ::: "c19", "c20");
}

/*
** clob3:
**	stp	c19, c20, \[csp, -48\]!
**	str	c21, \[csp, 32\]
**	ldr	c21, \[csp, 32\]
**	ldp	c19, c20, \[csp\], 48
**	ret
*/
void clob3(void)
{
  asm("" ::: "c19", "c20", "c21");
}

/* { dg-final { check-function-bodies "**" "" "" } } */
