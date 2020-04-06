/* PR c++/94314.  */
/* { dg-options "-O2 --param early-inlining-insns=100 -fdump-tree-cddce-details" } */

#include <stdio.h>

volatile int idx;
struct base
{
  __attribute__ ((malloc, noinline)) static void *
  operator new (unsigned long sz)
  {
    return ::operator new (sz);
  }

  __attribute__ ((malloc, noinline)) static void operator delete (void *ptr)
  {
    --count[idx];
    ::operator delete (ptr);
  }
  volatile static int count[2];
};
volatile int base::count[2] = {0, 0};
struct B : base
{
  static void *operator new (unsigned long sz)
  {
    ++count[idx];
    return base::operator new (sz);
  }
};

volatile int c = 1;

int
main ()
{
  for (int i; i < c; i++)
    {
      idx = 0;
      delete new B;
      if (B::count[0] != 0)
	__builtin_abort ();
    }

  return 0;
}
