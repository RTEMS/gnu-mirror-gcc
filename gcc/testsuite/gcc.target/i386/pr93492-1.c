/* { dg-do "compile" } */
/* { dg-options "-O1" } */

/* Note: this test only checks the instructions in the function bodies,
   not the placement of the patch label or nops before the function.  */

/*
**f10_none:
**	nop
**	ret
*/
void
__attribute__ ((patchable_function_entry (1, 0)))
f10_none (void)
{
}

/*
**f10_endbr:
**	endbr(32|64)
**	nop
**	ret
*/
void
__attribute__ ((patchable_function_entry (1, 0)))
f10_endbr (void)
{
}

/*
**f11_none:
**	ret
*/
void
__attribute__ ((patchable_function_entry (1, 1)))
f11_none (void)
{
}

/*
**f11_endbr:
**	endbr(32|64)
**	ret
*/
void
__attribute__ ((patchable_function_entry (1, 1)))
f11_endbr (void)
{
}

/*
**f21_none:
**	nop
**	ret
*/
void
__attribute__ ((patchable_function_entry (2, 1)))
f21_none (void)
{
}

/*
**f21_endbr:
**	endbr(32|64)
**	nop
**	ret
*/
void
__attribute__ ((patchable_function_entry (2, 1)))
f21_endbr (void)
{
}

/* { dg-final { scan-assembler "\.LPFE1:\n\tnop\n\trep ret" } } */
/* { dg-final { scan-assembler "\.LPFE2:\n\tnop\n\trep ret" } } */
/* { dg-final { scan-assembler "f11_none:\n\.LFB2:\n\t\.cfi_startproc\n\trep ret" } } */
