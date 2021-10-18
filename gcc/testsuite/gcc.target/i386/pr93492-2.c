/* { dg-do "compile" } */
/* { dg-options "-O1 -fasynchronous-unwind-tables -Wno-attributes" } */

/* Test the placement of the .LPFE1 label.  */

void
__attribute__ ((cf_check,patchable_function_entry (1, 0)))
f10_endbr (void)
{
}

/* { dg-final { scan-assembler "\.cfi_startproc\n.*\.LPFE1:\n\tnop\n\trep ret" } } */
