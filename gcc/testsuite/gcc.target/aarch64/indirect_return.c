/* { dg-do compile } */
/* { dg-options "-mbranch-protection=bti" } */

int __attribute((indirect_return)) foo (int a) 
{
  return a;
}

int
func1 (int a, int b)
{
  return foo (a + b);
}

/* { dg-final { scan-assembler-times "bti j" 1 } } */