/* { dg-do "compile" } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-O1 -mfentry -pg -fasynchronous-unwind-tables -Wno-attributes" } */

/* Test the placement of the .LPFE1 label.  */

void
__attribute__ ((cf_check,patchable_function_entry (1, 0)))
f10_endbr (void)
{
}

/* { dg-final { scan-assembler "\t\.cfi_startproc\n.*\.LPFE1:\n\tnop\n1:\tcall\t__fentry__\n\trep ret\n" } } */
