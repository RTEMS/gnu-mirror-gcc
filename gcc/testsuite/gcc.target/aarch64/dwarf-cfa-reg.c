/* Verify that CFA register is restored to SP after FP is restored.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gdwarf-2" } */
/* { dg-final { scan-assembler ".cfi_restore 30" { target { ! { cheri_capability_pure } } } } } */
/* { dg-final { scan-assembler ".cfi_restore 29" { target { ! { cheri_capability_pure } } } } } */
/* { dg-final { scan-assembler ".cfi_restore 228" { target { cheri_capability_pure } } } } */
/* { dg-final { scan-assembler ".cfi_restore 227" { target { cheri_capability_pure } } } } */
/* { dg-final { scan-assembler ".cfi_def_cfa_offset 0" } } */
/* { dg-final { scan-assembler "ret" } } */

int bar (unsigned int);

int foo (void)
{
  return bar (0xcafe);
}
