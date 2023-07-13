/* { dg-do compile } */
/* { dg-options "-O2 -mfold-mem-offsets" } */

void sink(int arr[2]);

void
foo(int a, int b, int i)
{
  int arr[2] = {a, b};
  arr[i]++;
  sink(arr);
}

// Should compile without negative memory offsets when using -mfold-mem-offsets
/* { dg-final { scan-assembler-not "lw\t.*,-.*\\(.*\\)" } } */
/* { dg-final { scan-assembler-not "sw\t.*,-.*\\(.*\\)" } } */