/* { dg-do run }

   PR tree-optimization/115494
   When trying to factor the redundant expression A | B below, the information
   that B has [0,1] range in the if branch was wrongly used to simplify the
   factored expression in the true FLAG case.  */

__attribute__((noipa))
unsigned f(_Bool flag, unsigned b, int a)
{
  int x;
  if (flag)
    a = 1;
  if ((b & 1) == b) // b [0,1]
    x = a | b;
  else
    x = a | b;
  return x;
}

int main()
{
  if (f(1, 3, 3) != 3)
    __builtin_abort();
}
