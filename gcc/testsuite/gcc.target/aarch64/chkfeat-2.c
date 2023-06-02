/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times {hint\t40 // chkfeat x16} 2 } } */

void bar (void);

/* Extern call may change enabled HW features.  */
unsigned long long
foo (void)
{
  unsigned long long a = __builtin_aarch64_chkfeat (1);
  bar ();
  unsigned long long b = __builtin_aarch64_chkfeat (1);
  return a + b;
}
