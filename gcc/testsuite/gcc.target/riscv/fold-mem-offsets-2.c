/* { dg-do compile } */
/* { dg-options "-O2 -mfold-mem-offsets" } */

void sink(int arr[3]);

void
foo(int a, int b, int c, int i)
{
  int arr1[3] = {a, b, c};
  int arr2[3] = {a, c, b};
  int arr3[3] = {c, b, a};

  arr1[i]++;
  arr2[i]++;
  arr3[i]++;
  
  sink(arr1);
  sink(arr2);
  sink(arr3);
}

// Should compile without negative memory offsets when using -mfold-mem-offsets
/* { dg-final { scan-assembler-not "lw\t.*,-.*\\(.*\\)" } } */
/* { dg-final { scan-assembler-not "sw\t.*,-.*\\(.*\\)" } } */