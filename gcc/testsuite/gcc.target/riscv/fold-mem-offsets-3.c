/* { dg-do compile } */
/* { dg-options "-O2 -mfold-mem-offsets" } */

void load(int arr[2]);

int
foo(long unsigned int i)
{
  int arr[2];
  load(arr);

  return arr[3 * i + 77];
}

// Should compile without negative memory offsets when using -mfold-mem-offsets
/* { dg-final { scan-assembler-not "lw\t.*,-.*\\(.*\\)" } } */
/* { dg-final { scan-assembler-not "addi\t.*,.*,77" } } */