/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=gcs" } */
/* { dg-final { scan-assembler-times "hint\\t40 // chkfeat x16" 2 } } */
/* { dg-final { scan-assembler-times "mrs\\tx\[0-9\]+, s3_3_c2_c5_1 // gcspr_el0" 2 } } */
/* { dg-final { scan-assembler-times "sysl\\txzr, #3, c7, c7, #1 // gcspopm" 1 } } */

int bar1 (int);
int bar2 (int);

void foo (int cmd)
{
  __label__ start;
  int x = 0;

  void nonlocal_goto (void)
  {
    x++;
    goto start;
  }

start:
  while (bar1 (x))
    if (bar2 (x))
      nonlocal_goto ();
}
